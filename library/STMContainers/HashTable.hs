module STMContainers.HashTable where

import STMContainers.Prelude
import Data.Primitive.Array
import qualified STMContainers.WordArray as WordArray
import qualified STMContainers.HashTable.Node as Node


-- |
-- A hash array mapped trie, specialized for STM.
type HashTable k v = Node.Node k v

-- |
-- A constraint for keys.
type IsKey k = (Eq k, Hashable k)

insert :: (IsKey k) => k -> v -> HashTable k v -> STM ()
insert k v t = Node.insert (hash k, k) v (0, t)

delete :: (IsKey k) => k -> HashTable k v -> STM ()
delete k t = void $ Node.delete (hash k, k) (0, t)

lookup :: (IsKey k) => k -> HashTable k v -> STM (Maybe v)
lookup k t = Node.lookup (hash k, k) (0, t)




