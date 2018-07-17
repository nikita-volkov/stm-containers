module APITests.MapTests.Update where

import Test.Framework
import BasePrelude hiding (insert, delete, update)
import MTLPrelude
import Control.Monad.Free
import Control.Monad.Free.TH


data UpdateF k v c =
  Insert k v c |
  Delete k c |
  Adjust (v -> v) k c
  deriving (Functor)

instance (Show k, Show v, Show c) => Show (UpdateF k v c) where
  showsPrec i = 
    showParen (i > 5) . \case
      Insert k v c -> 
        showString "Insert " . 
        showsPrecInner k .
        showChar ' ' .
        showsPrecInner v .
        showChar ' ' .
        showsPrecInner c
      Delete k c ->
        showString "Delete " .
        showsPrecInner k .
        showChar ' ' .
        showsPrecInner c
      Adjust f k c ->
        showString "Adjust " .
        showString "<v -> v> " .
        showsPrecInner k .
        showChar ' ' .
        showsPrecInner c
    where
      showsPrecInner = showsPrec (succ 5)

instance Show1 (UpdateF k v)

makeFree ''UpdateF

type Update k v = Free (UpdateF k v) ()

instance (Arbitrary k, Arbitrary v) => Arbitrary (Update k v) where
  arbitrary = 
    frequency
      [
        (1 , delete <$> arbitrary),
        (10, insert <$> arbitrary <*> arbitrary),
        (3 , adjust <$> (const <$> arbitrary) <*> arbitrary)
      ]

