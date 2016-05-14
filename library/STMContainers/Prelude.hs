module STMContainers.Prelude
( 
  module Exports,
  traversePair,
)
where

-- base
-------------------------
import BasePrelude as Exports

-- hashable
-------------------------
import Data.Hashable as Exports (Hashable(..))

-- transformers
-------------------------
import Control.Monad.Trans.Class as Exports

-- list-t
-------------------------
import ListT as Exports (ListT)

-- | A replacement for the missing 'Traverse' instance of pair in base < 4.7.
traversePair :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
traversePair f (x, y) = (,) x <$> f y
