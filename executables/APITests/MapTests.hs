{-# OPTIONS_GHC -F -pgmF htfpp #-}
module APITests.MapTests where

import Test.Framework
import STMContainers.Prelude
import STMContainers.Transformers
import Control.Monad.Free
import qualified APITests.MapTests.Update as Update
import qualified STMContainers.Map as STMMap
import qualified Focus
import qualified Data.HashMap.Strict as HashMap


interpretSTMMapUpdate :: (Hashable k, Eq k) => Update.Update k v -> STM (STMMap.Map k v)
interpretSTMMapUpdate update = do
  m <- STMMap.new
  flip iterM update $ \case
    Update.Insert k v c -> STMMap.insert k v m >> c
    Update.Delete k c   -> STMMap.delete k m >> c
    Update.Adjust f k c -> STMMap.focus ((Focus.adjustM . fmap return) f) k m >> c
  return m

interpretHashMapUpdate :: (Hashable k, Eq k) => Update.Update k v -> HashMap.HashMap k v
interpretHashMapUpdate update = 
  flip execState HashMap.empty $ flip iterM update $ \case
    Update.Insert k v c -> modify (HashMap.insert k v) >> c
    Update.Delete k c   -> modify (HashMap.delete k) >> c
    Update.Adjust f k c -> modify (adjust f k) >> c
  where
    adjust f k m = 
      case HashMap.lookup k m of
        Nothing -> m
        Just a -> HashMap.insert k (f a) m

stmMapToHashMap :: (Hashable k, Eq k) => STMMap.Map k v -> STM (HashMap.HashMap k v)
stmMapToHashMap = STMMap.foldM f HashMap.empty
  where
    f m (STMMap.Association k v) = return (HashMap.insert k v m)

stmMapFromList :: (Hashable k, Eq k) => [(k, v)] -> STM (STMMap.Map k v)
stmMapFromList list = do
  m <- STMMap.new
  forM_ list $ \(k, v) -> STMMap.insert k v m
  return m

interpretSTMMapUpdateAsHashMap :: (Hashable k, Eq k) => Update.Update k v -> HashMap.HashMap k v
interpretSTMMapUpdateAsHashMap =
  unsafePerformIO . atomically . (stmMapToHashMap <=< interpretSTMMapUpdate)


-- * Intentional hash collision simulation
-------------------------

newtype TestKey = TestKey Word8
  deriving (Eq, Show)

instance Arbitrary TestKey where
  arbitrary = TestKey <$> choose (0, 63)

instance Hashable TestKey where
  hashWithSalt salt (TestKey w) =
    if odd w
      then hashWithSalt salt (pred w)
      else hashWithSalt salt w


-- * Tests
-------------------------

prop_fromListToListIsomorphism =
  forAll gen prop
  where
    gen = do
      keys <- nub <$> listOf (arbitrary :: Gen Char)
      mapM (liftA2 (flip (,)) (arbitrary :: Gen Int) . pure) keys
    prop list =
      list \\ list' === []
      where
        list' = unsafePerformIO $ atomically $ 
          stmMapFromList list >>= STMMap.toList >>= return . map (\(STMMap.Association k v) -> (k, v))

prop_updatesProduceTheSameEffectAsInHashMap =
  withQCArgs (\a -> a {maxSuccess = 1000}) prop
  where
    prop (updates :: [Update.Update TestKey ()]) =
      interpretHashMapUpdate update === interpretSTMMapUpdateAsHashMap update
      where
        update = sequence_ updates

test_insert = do
  assertEqual (HashMap.fromList [('a', 1), ('b', 2), ('c', 3)]) =<< do 
    atomically $ do
      m <- STMMap.new
      STMMap.insert 'a' 1 m
      STMMap.insert 'c' 3 m
      STMMap.insert 'b' 2 m
      stmMapToHashMap m

test_insert2 = do
  assertEqual (HashMap.fromList [(111 :: Int, ()), (207, ())]) =<< do 
    atomically $ do
      m <- STMMap.new
      STMMap.insert 111 () m
      STMMap.insert 207 () m
      stmMapToHashMap m

test_adjust = do
  assertEqual (HashMap.fromList [('a', 1), ('b', 3)]) =<< do 
    atomically $ do
      m <- stmMapFromList [('a', 1), ('b', 2)]
      STMMap.focus (Focus.adjustM (const $ return 3)) 'b' m
      stmMapToHashMap m

test_focusReachesTheTarget = do
  assertEqual (Just 2) =<< do 
    atomically $ do
      m <- stmMapFromList [('a', 1), ('b', 2)]
      STMMap.focus Focus.lookupM 'b' m
