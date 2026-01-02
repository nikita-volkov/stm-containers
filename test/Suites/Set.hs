module Suites.Set (tests) where

import qualified Control.Foldl as Foldl
import Control.Monad.Free
import qualified DeferredFolds.UnfoldlM as UnfoldlM
import qualified Focus
import qualified StmContainers.Set as StmSet
import qualified ListT
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Prelude hiding (null, choose)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (forM_)
import Control.Concurrent.STM
import Data.Hashable
import Data.List (nub, sort, splitAt)
import Data.Word (Word8)

-- helpers

stmSetFromList :: (Hashable a, Eq a) => [a] -> STM (StmSet.Set a)
stmSetFromList xs = do
  s <- StmSet.new
  forM_ xs $ \x -> StmSet.insert x s
  return s

stmSetToList :: StmSet.Set a -> STM [a]
stmSetToList = UnfoldlM.foldM (Foldl.generalize Foldl.list) . StmSet.unfoldlM

-- * Intentional hash collision simulation

newtype TestKey = TestKey Word8
  deriving (Eq, Ord, Show)

instance Arbitrary TestKey where
  arbitrary = TestKey <$> choose (0, 63)

instance Hashable TestKey where
  hashWithSalt salt (TestKey w) =
    if odd w
      then hashWithSalt salt (pred w)
      else hashWithSalt salt w

-- * Tests

tests :: [TestTree]
tests =
  [ testProperty "sizeAndList" $
      let gen = nub <$> listOf (choose ('a', 'z'))
          prop xs =
            length xs == stmSetSize
            where
              stmSetSize =
                unsafePerformIO $ atomically $ do
                  s <- stmSetFromList xs
                  StmSet.size s
       in forAll gen prop,
    testProperty "fromListToListSetIsomorphism" $ \(xs :: [Int]) ->
      let setList =
            unsafePerformIO $ atomically $
              stmSetFromList xs >>= stmSetToList
       in sort (nub xs) === sort setList,
    testProperty "listTNonAtomicIsomorphism" $ \(xs :: [Int]) ->
      let setList =
            unsafePerformIO $ do
              set <- atomically (stmSetFromList xs)
              ListT.toList (StmSet.listTNonAtomic set)
       in sort (nub xs) === sort setList,
    testProperty "insertDeleteWithCollisions" $ \(ks :: [TestKey]) ->
      let dropped = take (length ks `div` 2) ks
          (finalSize, finalList) =
            unsafePerformIO $ atomically $ do
              s <- StmSet.new
              -- insert all
              forM_ ks $ \k -> StmSet.insert k s
              -- delete ~the first half of them
              forM_ dropped $ \k -> StmSet.delete k s
              sz <- StmSet.size s
              ls <- stmSetToList s
              return (sz, sort ls)
          expected =
            let remaining = nub (filter (`notElem` dropped) ks)
            in (length remaining, sort remaining)
       in (finalSize, finalList) === expected,
    testCase "insert" $
      assertEqual "" (sort ['a','b','c']) =<< do
        atomically $ do
          s <- StmSet.new
          StmSet.insert 'a' s
          StmSet.insert 'c' s
          StmSet.insert 'b' s
          sort <$> stmSetToList s,
    testCase "focusInsert" $
      assertEqual "" (sort ['a','b']) =<< do
        atomically $ do
          s <- StmSet.new
          StmSet.focus (Focus.insert ()) 'a' s
          StmSet.focus (Focus.insert ()) 'b' s
          sort <$> stmSetToList s,
    testCase "insertAndDelete" $
      assertEqual "" ['b'] =<< do
        atomically $ do
          s <- StmSet.new
          StmSet.focus (Focus.insert ()) 'a' s
          StmSet.focus (Focus.insert ()) 'b' s
          StmSet.focus Focus.delete 'a' s
          sort <$> stmSetToList s,
    testCase "nullAndNotNull" $ do
      assertEqual "" True =<< atomically (StmSet.null =<< StmSet.new)
      assertEqual "" False =<< atomically (StmSet.null =<< stmSetFromList ['a']),
    testCase "nullAfterDeletingTheLastElement" $
      assertEqual "" True =<< do
        atomically $ do
          s <- stmSetFromList ['a']
          StmSet.delete 'a' s
          StmSet.null s
  ]
