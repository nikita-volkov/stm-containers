module STMContainers.HashTable where

import STMContainers.Prelude
import Data.Primitive.Array
import qualified STMContainers.WordArray as WordArray
import qualified STMContainers.HashTable.Node as Node


-- |
-- A hash array mapped trie, specialized for STM.
type HashTable k v = Node.Node k v
type IsKey k = (Eq k, Hashable k)

insert :: (IsKey k) => HashTable k v -> k -> v -> STM ()
insert t k v = Node.insert (0, t) (hash k, k) v

delete :: (IsKey k) => HashTable k v -> k -> STM ()
delete t k = void $ Node.delete (0, t) (hash k, k)




