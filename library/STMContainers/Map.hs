module STMContainers.Map where

import STMContainers.Prelude
import qualified STMContainers.HAMT as HAMT
import qualified STMContainers.HAMT.Node as HAMTNode


-- |
-- A hash table, based on an STM-specialized hash array mapped trie.
type Map k v = HAMT.HAMT (Association k v)

-- |
-- A constraint for keys.
type Key k = (Eq k, Hashable k)

-- |
-- A key-value association.
data Association k v = Association !k !v

instance (Eq k) => HAMTNode.Element (Association k v) where
  type ElementIndex (Association k v) = k
  elementIndex (Association k v) = k

associationValue :: Association k v -> v
associationValue (Association _ v) = v

insert :: (Key k) => k -> v -> Map k v -> STM ()
insert k v = inline HAMT.insert (Association k v)

delete :: (Key k) => k -> Map k v -> STM ()
delete = inline HAMT.delete

lookup :: (Key k) => k -> Map k v -> STM (Maybe v)
lookup k = (fmap . fmap) associationValue . inline HAMT.lookup k


