{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main.BimapTests where

import Prelude
import Test.Framework
import StmContainers.Bimap
import qualified Focus

test_insertOverwrites = do
  m <- newIO :: IO (Bimap Int Int)
  atomically $ insertRight 3 1 m
  atomically $ insertRight 3 2 m
  assertEqual Nothing =<< atomically (lookupRight 1 m)
  assertEqual (Just 3) =<< atomically (lookupRight 2 m)
  assertEqual (Just 2) =<< atomically (lookupLeft 3 m)
