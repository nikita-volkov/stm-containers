module STMContainers.Map where

import STMContainers.Prelude
import qualified STMContainers.HAMT as HAMT


-- |
-- A hash table, based on an STM-specialized hash array mapped trie.
type Map k v = HAMT.Node k v

-- |
-- A constraint for keys.
type IsKey k = (Eq k, Hashable k)

insert :: (IsKey k) => k -> v -> Map k v -> STM ()
insert k v t = HAMT.insert (hash k, k) v (0, t)

delete :: (IsKey k) => k -> Map k v -> STM ()
delete k t = void $ HAMT.delete (hash k, k) (0, t)

lookup :: (IsKey k) => k -> Map k v -> STM (Maybe v)
lookup k t = HAMT.lookup (hash k, k) (0, t)




