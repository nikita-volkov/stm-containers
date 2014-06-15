module STMContainers.HashTable where

import STMContainers.Prelude
import Data.Primitive.Array
import qualified STMContainers.WordArray as WordArray


type HashTable k v = TVar (Node k v)

data Node k v =
  Empty |
  NodesWordArray {-# UNPACK #-} !(WordArray (HashTable k v)) |
  NodesFullArray {-# UNPACK #-} !(Array (HashTable k v)) |
  SingleLeaf {-# UNPACK #-} !Hash !k !v |
  MultipleLeaves {-# UNPACK #-} !Hash {-# UNPACK #-} !(Array (k, v))

type WordArray = WordArray.WordArray

type Hash = Int

type Key k = (Eq k, Hashable k)

type Depth = Int
