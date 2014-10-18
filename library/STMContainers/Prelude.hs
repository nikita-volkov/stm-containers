module STMContainers.Prelude
( 
  module Exports,
  bug,
  bottom,
  traversePair,
)
where

-- base
-------------------------
import BasePrelude as Exports

-- placeholders
-------------------------
import Development.Placeholders as Exports

-- hashable
-------------------------
import Data.Hashable as Exports (Hashable(..))

-- transformers
-------------------------
import Control.Monad.Trans.Class as Exports

-- list-t
-------------------------
import ListT as Exports (ListT)

-- custom
-------------------------
import qualified Debug.Trace.LocationTH

bug = [e| $(Debug.Trace.LocationTH.failure) . (msg <>) |]
  where
    msg = "A \"stm-containers\" package bug: " :: String

bottom = [e| $bug "Bottom evaluated" |]

-- | A replacement for the missing 'Traverse' instance of pair in base < 4.7.
traversePair :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
traversePair f (x, y) = (,) x <$> f y
