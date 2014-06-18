module STMContainers.HashTable where

import STMContainers.Prelude
import qualified STMContainers.HAMT as HAMT


-- |
-- A hash array mapped trie, specialized for STM.
type HashTable k v = HAMT.Node k v

-- |
-- A constraint for keys.
type IsKey k = (Eq k, Hashable k)

insert :: (IsKey k) => k -> v -> HashTable k v -> STM ()
insert k v t = HAMT.insert (hash k, k) v (0, t)

delete :: (IsKey k) => k -> HashTable k v -> STM ()
delete k t = void $ HAMT.delete (hash k, k) (0, t)

lookup :: (IsKey k) => k -> HashTable k v -> STM (Maybe v)
lookup k t = HAMT.lookup (hash k, k) (0, t)




