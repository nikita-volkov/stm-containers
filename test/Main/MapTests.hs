{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main.MapTests where

import Prelude hiding (choose)
import Test.Framework
import Control.Monad.Free
import qualified Main.MapTests.Update as Update
import qualified StmContainers.Map as StmMap
import qualified Focus
import qualified Data.HashMap.Strict as HashMap
import qualified DeferredFolds.UnfoldM as UnfoldM
import qualified Control.Foldl as Foldl


interpretStmMapUpdate :: (Hashable k, Eq k) => Update.Update k v -> STM (StmMap.Map k v)
interpretStmMapUpdate update = do
  m <- StmMap.new
  flip iterM update $ \case
    Update.Insert k v c -> StmMap.insert v k m >> c
    Update.Delete k c   -> StmMap.delete k m >> c
    Update.Adjust f k c -> StmMap.focus ((Focus.adjustM . fmap return) f) k m >> c
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

stmMapToHashMap :: (Hashable k, Eq k) => StmMap.Map k v -> STM (HashMap.HashMap k v)
stmMapToHashMap = UnfoldM.foldM (Foldl.generalize Foldl.hashMap) . StmMap.unfoldM

stmMapFromList :: (Hashable k, Eq k) => [(k, v)] -> STM (StmMap.Map k v)
stmMapFromList list = do
  m <- StmMap.new
  forM_ list $ \(k, v) -> StmMap.insert v k m
  return m

stmMapToList :: StmMap.Map k v -> STM [(k, v)]
stmMapToList = UnfoldM.foldM (Foldl.generalize Foldl.list) . StmMap.unfoldM

interpretStmMapUpdateAsHashMap :: (Hashable k, Eq k) => Update.Update k v -> HashMap.HashMap k v
interpretStmMapUpdateAsHashMap =
  unsafePerformIO . atomically . (stmMapToHashMap <=< interpretStmMapUpdate)


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
      keys <- nub <$> listOf (choose ('a', 'z'))
      mapM (liftA2 (flip (,)) (choose (0, 99 :: Int)) . pure) keys
    prop list =
      length list == stmMapLength
      where
        stmMapLength =
          unsafePerformIO $ atomically $ do
            x <- stmMapFromList list
            StmMap.size x

prop_fromUnfoldMoListIsomorphism =
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
      interpretHashMapUpdate update === interpretStmMapUpdateAsHashMap update
      where
        update = sequence_ updates

test_focusInsert = do
  assertEqual (HashMap.fromList [('a', 1), ('b', 2)]) =<< do 
    atomically $ do
      m <- StmMap.new
      StmMap.focus (Focus.insert 1) 'a' m
      StmMap.focus (Focus.insert 2) 'b' m
      stmMapToHashMap m

test_focusInsertAndDelete = do
  assertEqual (HashMap.fromList [('b', 2)]) =<< do 
    atomically $ do
      m <- StmMap.new
      StmMap.focus (Focus.insert 1) 'a' m
      StmMap.focus (Focus.insert 2) 'b' m
      StmMap.focus (Focus.delete) 'a' m
      stmMapToHashMap m

test_focusInsertAndDeleteWithCollision = do
  assertEqual (HashMap.fromList [(TestKey 32, 2)]) =<< do 
    atomically $ do
      m <- StmMap.new
      StmMap.focus (Focus.insert 2) (TestKey 32) m
      StmMap.focus (Focus.delete) (TestKey 1) m
      stmMapToHashMap m

test_insert = do
  assertEqual (HashMap.fromList [('a', 1), ('b', 2), ('c', 3)]) =<< do 
    atomically $ do
      m <- StmMap.new
      StmMap.insert 1 'a' m
      StmMap.insert 3 'c' m
      StmMap.insert 2 'b' m
      stmMapToHashMap m

test_insert2 = do
  assertEqual (HashMap.fromList [(111 :: Int, ()), (207, ())]) =<< do 
    atomically $ do
      m <- StmMap.new
      StmMap.insert () 111 m
      StmMap.insert () 207 m
      stmMapToHashMap m

test_adjust = do
  assertEqual (HashMap.fromList [('a', 1), ('b', 3)]) =<< do 
    atomically $ do
      m <- stmMapFromList [('a', 1), ('b', 2)]
      StmMap.focus (Focus.adjustM (const $ return 3)) 'b' m
      stmMapToHashMap m

test_focusReachesTheTarget = do
  assertEqual (Just 2) =<< do 
    atomically $ do
      m <- stmMapFromList [('a', 1), ('b', 2)]
      StmMap.focus Focus.lookup 'b' m

test_notNull = do
  assertEqual False =<< do 
    atomically $ StmMap.null =<< stmMapFromList [('a', ())]

test_nullAfterDeletingTheLastElement = do
  assertEqual True =<< do 
    atomically $ do
      m <- stmMapFromList [('a', ())]
      StmMap.delete 'a' m
      StmMap.null m
