module Suites.Bimap (tests) where

import qualified Focus
import qualified ListT
import StmContainers.Bimap
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

tests =
  [ testCase "construction" $ do
      m <- newIO :: IO (Bimap Int Int)
      atomically $ insertRight 3 1 m
      atomically $ insertRight 4 2 m
      assertEqual "" [(3, 1), (4, 2)] =<< atomically (ListT.toList (listT m)),
    testCase "deleteLeft" $ do
      m <- newIO :: IO (Bimap Int Int)
      atomically $ insertRight 3 1 m
      atomically $ insertRight 4 2 m
      atomically $ deleteLeft 4 m
      assertEqual "" [(3, 1)] =<< atomically (ListT.toList (listT m)),
    testCase "deleteRight" $ do
      m <- newIO :: IO (Bimap Int Int)
      atomically $ insertRight 3 1 m
      atomically $ insertRight 4 2 m
      atomically $ deleteRight 2 m
      assertEqual "" [(3, 1)] =<< atomically (ListT.toList (listT m)),
    testCase "replacing construction" $ do
      m <- newIO :: IO (Bimap Int Int)
      atomically $ insertRight 3 1 m
      atomically $ insertRight 4 2 m
      atomically $ insertRight 3 2 m
      assertEqual "" [(3, 2)] =<< atomically (ListT.toList (listT m)),
    testCase "insert overwrites" $ do
      m <- newIO :: IO (Bimap Int Int)
      atomically $ insertRight 3 1 m
      assertEqual "" 1 =<< atomically (size m)
      atomically $ insertRight 3 2 m
      assertEqual "" 1 =<< atomically (size m)
      assertEqual "" Nothing =<< atomically (lookupRight 1 m)
      assertEqual "" (Just 3) =<< atomically (lookupRight 2 m)
      assertEqual "" (Just 2) =<< atomically (lookupLeft 3 m)
      assertEqual "" Nothing =<< atomically (focusRight Focus.lookup 1 m)
      assertEqual "" (Just 3) =<< atomically (focusRight Focus.lookup 2 m)
      atomically $ focusRight (Focus.insert 3) 4 m
      assertEqual "" 1 =<< atomically (size m)
      assertEqual "" Nothing =<< atomically (lookupRight 1 m)
      assertEqual "" Nothing =<< atomically (lookupRight 2 m)
      assertEqual "" (Just 3) =<< atomically (lookupRight 4 m),
    testCase "insert overwrites 2" $ do
      m <- newIO :: IO (Bimap Int Char)
      atomically $ insertLeft 'a' 1 m
      assertEqual "" 1 =<< atomically (size m)
      atomically $ insertLeft 'a' 2 m
      assertEqual "" 1 =<< atomically (size m)
      assertEqual "" Nothing =<< atomically (lookupLeft 1 m)
      assertEqual "" (Just 'a') =<< atomically (lookupLeft 2 m)
      assertEqual "" Nothing =<< atomically (focusLeft Focus.lookup 1 m)
      assertEqual "" (Just 'a') =<< atomically (focusLeft Focus.lookup 2 m)
      atomically $ focusLeft (Focus.insert 'a') 3 m
      assertEqual "" 1 =<< atomically (size m)
      assertEqual "" Nothing =<< atomically (lookupLeft 1 m)
      assertEqual "" Nothing =<< atomically (lookupLeft 2 m)
      assertEqual "" (Just 'a') =<< atomically (lookupLeft 3 m)
  ]
