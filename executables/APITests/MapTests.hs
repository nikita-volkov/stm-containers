{-# OPTIONS_GHC -F -pgmF htfpp #-}
module APITests.MapTests where

import Test.Framework
import STMContainers.Prelude
import STMContainers.Transformers
import Control.Monad.Free
import qualified APITests.MapTests.Update as Update
import qualified STMContainers.Map as STMMap
import qualified STMContainers.Visit as Visit
import qualified Data.HashMap.Strict as HashMap


interpretSTMMapUpdate :: (Hashable k, Eq k) => Update.Update k v -> STM (STMMap.Map k v)
interpretSTMMapUpdate update = do
  m <- STMMap.new
  flip iterM update $ \case
    Update.Insert k v c -> STMMap.insert k v m >> c
    Update.Delete k c   -> STMMap.delete k m >> c
    Update.Adjust f k c -> STMMap.visit ((Visit.monadize . Visit.adjust) f) k m >> c
  return m

interpretHashMapUpdate :: (Hashable k, Eq k) => Update.Update k v -> HashMap.HashMap k v
interpretHashMapUpdate update = 
  flip execState HashMap.empty $ flip iterM update $ \case
    Update.Insert k v c -> modify (HashMap.insert k v) >> c
    Update.Delete k c   -> modify (HashMap.delete k) >> c
    Update.Adjust f k c -> modify (HashMap.insert k (f undefined)) >> c

stmMapToHashMap :: (Hashable k, Eq k) => STMMap.Map k v -> STM (HashMap.HashMap k v)
stmMapToHashMap = STMMap.foldM f HashMap.empty
  where
    f m (STMMap.Association k v) = return (HashMap.insert k v m)

stmMapFromList :: (Hashable k, Eq k) => [(k, v)] -> STM (STMMap.Map k v)
stmMapFromList list = do
  m <- STMMap.new
  forM_ list $ \(k, v) -> STMMap.insert k v m
  return m


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

prop_updatesProduceTheSameEffectAsInHashMap (updates :: [Update.Update Word8 Char]) =
  interpretHashMapUpdate update ===
  (unsafePerformIO . atomically . (stmMapToHashMap <=< interpretSTMMapUpdate)) update
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

test_adjust = do
  assertEqual (HashMap.fromList [('a', 1), ('b', 3)]) =<< do 
    atomically $ do
      m <- stmMapFromList [('a', 1), ('b', 2)]
      STMMap.visit (Visit.adjustM (const $ return 3)) 'b' m
      stmMapToHashMap m

test_visitReachesTheTarget = do
  assertEqual (Just 2) =<< do 
    atomically $ do
      m <- stmMapFromList [('a', 1), ('b', 2)]
      STMMap.visit (Visit.monadize Visit.lookup) 'b' m

