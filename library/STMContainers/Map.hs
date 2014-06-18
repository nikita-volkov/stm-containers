module STMContainers.Map where

import STMContainers.Prelude
import qualified STMContainers.HAMT as HAMT


-- |
-- A hash table, based on an STM-specialized hash array mapped trie.
type Map k v = HAMT.Node (Association k v)

-- |
-- A constraint for keys.
type Key k = (Eq k, Hashable k)

-- |
-- A key-value association.
data Association k v = Association !k !v

instance (Eq k) => HAMT.Element (Association k v) where
  type Index (Association k v) = k
  elementIndex (Association k v) = k

insert :: (Key k) => k -> v -> Map k v -> STM ()
insert k v t = HAMT.insert (hash k, k) (Association k v) (0, t)

delete :: (Key k) => k -> Map k v -> STM ()
delete k t = void $ HAMT.delete (hash k, k) (0, t)

lookup :: (Key k) => k -> Map k v -> STM (Maybe v)
lookup k t = (fmap . fmap) associationValue $ HAMT.lookup (hash k, k) (0, t)

associationValue :: Association k v -> v
associationValue (Association _ v) = v



