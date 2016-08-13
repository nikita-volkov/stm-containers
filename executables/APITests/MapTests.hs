{-# OPTIONS_GHC -F -pgmF htfpp #-}
module APITests.MapTests where

import Test.Framework
import BasePrelude
import MTLPrelude
import Data.Hashable
import Control.Monad.Free
import qualified APITests.MapTests.Update as Update
import qualified STM.Containers.Map as STMMap
import qualified Focus.Impure as Focus
import qualified Data.HashMap.Strict as HashMap
import qualified ListT


interpretSTMMapUpdate :: (Hashable k, Eq k) => Update.Update k v -> STM (STMMap.Map k v)
interpretSTMMapUpdate update = do
  m <- STMMap.new
  flip iterM update $ \case
    Update.Insert k v c -> STMMap.insert v k m >> c
    Update.Delete k c   -> STMMap.delete k m >> c
    Update.Adjust f k c -> STMMap.focus ((Focus.adjust . fmap return) f) k m >> c
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
stmMapToHashMap = ListT.fold f HashMap.empty . STMMap.stream
  where
    f m (k, v) = return (HashMap.insert k v m)

stmMapFromList :: (Hashable k, Eq k) => [(k, v)] -> STM (STMMap.Map k v)
stmMapFromList list = do
  m <- STMMap.new
  forM_ list $ \(k, v) -> STMMap.insert v k m
  return m

stmMapToList :: STMMap.Map k v -> STM [(k, v)]
stmMapToList = ListT.fold (\l -> return . (:l)) [] . STMMap.stream

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

prop_sizeAndList =
  forAll gen prop
  where
    gen = do
      keys <- nub <$> listOf (arbitrary :: Gen Char)
      mapM (liftA2 (flip (,)) (arbitrary :: Gen Int) . pure) keys
    prop list =
      length list == stmMapLength
      where
        stmMapLength =
          unsafePerformIO $ atomically $ do
            x <- stmMapFromList list
            STMMap.size x

prop_fromListToListIsomorphism =
  forAll gen prop
  where
    gen = do
      keys <- nub <$> listOf (arbitrary :: Gen Char)
      mapM (liftA2 (flip (,)) (arbitrary :: Gen Int) . pure) keys
    prop list =
      list \\ list' === []
      where
        list' = unsafePerformIO $ atomically $ stmMapFromList list >>= stmMapToList

prop_updatesProduceTheSameEffectAsInHashMap =
  withQCArgs (\a -> a {maxSuccess = 1000}) prop
  where
    prop (updates :: [Update.Update TestKey ()]) =
      interpretHashMapUpdate update === interpretSTMMapUpdateAsHashMap update
      where
        update = sequence_ updates

test_focusInsert = do
  assertEqual (HashMap.fromList [('a', 1), ('b', 2)]) =<< do 
    atomically $ do
      m <- STMMap.new
      STMMap.focus (Focus.insert 1) 'a' m
      STMMap.focus (Focus.insert 2) 'b' m
      stmMapToHashMap m

test_focusInsertAndDelete = do
  assertEqual (HashMap.fromList [('b', 2)]) =<< do 
    atomically $ do
      m <- STMMap.new
      STMMap.focus (Focus.insert 1) 'a' m
      STMMap.focus (Focus.insert 2) 'b' m
      STMMap.focus (Focus.delete) 'a' m
      stmMapToHashMap m

test_focusInsertAndDeleteWithCollision = do
  assertEqual (HashMap.fromList [(TestKey 32, 2)]) =<< do 
    atomically $ do
      m <- STMMap.new
      STMMap.focus (Focus.insert 2) (TestKey 32) m
      STMMap.focus (Focus.delete) (TestKey 1) m
      stmMapToHashMap m

test_insert = do
  assertEqual (HashMap.fromList [('a', 1), ('b', 2), ('c', 3)]) =<< do 
    atomically $ do
      m <- STMMap.new
      STMMap.insert 1 'a' m
      STMMap.insert 3 'c' m
      STMMap.insert 2 'b' m
      stmMapToHashMap m

test_insert2 = do
  assertEqual (HashMap.fromList [(111 :: Int, ()), (207, ())]) =<< do 
    atomically $ do
      m <- STMMap.new
      STMMap.insert () 111 m
      STMMap.insert () 207 m
      stmMapToHashMap m

test_adjust = do
  assertEqual (HashMap.fromList [('a', 1), ('b', 3)]) =<< do 
    atomically $ do
      m <- stmMapFromList [('a', 1), ('b', 2)]
      STMMap.focus (Focus.adjust (const $ return 3)) 'b' m
      stmMapToHashMap m

test_focusReachesTheTarget = do
  assertEqual (Just 2) =<< do 
    atomically $ do
      m <- stmMapFromList [('a', 1), ('b', 2)]
      STMMap.focus Focus.lookup 'b' m

test_notNull = do
  assertEqual False =<< do 
    atomically $ STMMap.null =<< stmMapFromList [('a', ())]

test_nullAfterDeletingTheLastElement = do
  assertEqual True =<< do 
    atomically $ do
      m <- stmMapFromList [('a', ())]
      STMMap.delete 'a' m
      STMMap.null m

