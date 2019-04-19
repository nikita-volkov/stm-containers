{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main.BimapTests where

import Prelude
import Test.Framework
import StmContainers.Bimap
import qualified Focus
import qualified ListT

test_construction = do
  m <- newIO :: IO (Bimap Int Int)
  atomically $ insertRight 3 1 m
  atomically $ insertRight 4 2 m
  assertEqual [(3, 1), (4, 2)] =<< atomically (ListT.toList (listT m))

test_deleteLeft = do
  m <- newIO :: IO (Bimap Int Int)
  atomically $ insertRight 3 1 m
  atomically $ insertRight 4 2 m
  atomically $ deleteLeft 4 m
  assertEqual [(3, 1)] =<< atomically (ListT.toList (listT m))

test_deleteRight = do
  m <- newIO :: IO (Bimap Int Int)
  atomically $ insertRight 3 1 m
  atomically $ insertRight 4 2 m
  atomically $ deleteRight 2 m
  assertEqual [(3, 1)] =<< atomically (ListT.toList (listT m))

test_replactingConstruction = do
  m <- newIO :: IO (Bimap Int Int)
  atomically $ insertRight 3 1 m
  atomically $ insertRight 4 2 m
  atomically $ insertRight 3 2 m
  assertEqual [(3, 2)] =<< atomically (ListT.toList (listT m))

test_insertOverwrites = do
  m <- newIO :: IO (Bimap Int Int)
  atomically $ insertRight 3 1 m
  atomically $ insertRight 3 2 m
  assertEqual Nothing =<< atomically (lookupRight 1 m)
  assertEqual (Just 3) =<< atomically (lookupRight 2 m)
  assertEqual (Just 2) =<< atomically (lookupLeft 3 m)
  assertEqual Nothing =<< atomically (focusRight Focus.lookup 1 m)
  assertEqual (Just 3) =<< atomically (focusRight Focus.lookup 2 m)
  atomically $ focusRight (Focus.insert 3) 4 m
  assertEqual Nothing =<< atomically (lookupRight 1 m)
  assertEqual Nothing =<< atomically (lookupRight 2 m)
  assertEqual (Just 3) =<< atomically (lookupRight 4 m)
  assertEqual 1 =<< atomically (size m)

