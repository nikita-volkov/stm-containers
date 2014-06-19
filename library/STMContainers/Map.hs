module STMContainers.Map
(
  Map,
  Indexable,
  Association(..),
  new,
  insert,
  delete,
  alter,
  lookup,
  foldM,
  toList,
)
where

import STMContainers.Prelude hiding (insert, delete, lookup, alter, foldM, toList, empty)
import qualified STMContainers.HAMT as HAMT
import qualified STMContainers.HAMT.Node as HAMTNode
import qualified STMContainers.Alter as Alter


-- |
-- A hash table, based on an STM-specialized hash array mapped trie.
type Map k v = HAMT.HAMT (Association k v)

-- |
-- A standard constraint for keys.
type Indexable a = (Eq a, Hashable a)

-- |
-- A key-value association.
data Association k v = Association !k !v

instance (Eq k) => HAMTNode.Element (Association k v) where
  type ElementIndex (Association k v) = k
  elementIndex (Association k v) = k

associationValue :: Association k v -> v
associationValue (Association _ v) = v

associationToTuple :: Association k v -> (k, v)
associationToTuple (Association k v) = (k, v)

lookup :: (Indexable k) => k -> Map k v -> STM (Maybe v)
lookup k = (fmap . fmap) associationValue . inline HAMT.lookup k

insert :: (Indexable k) => k -> v -> Map k v -> STM ()
insert k v = inline HAMT.insert (Association k v)

delete :: (Indexable k) => k -> Map k v -> STM ()
delete = inline HAMT.delete

alter :: (Indexable k) => (Alter.AlterM STM v r) -> k -> Map k v -> STM r
alter f k = inline HAMT.alter f' k
  where
    f' = (fmap . fmap . fmap) (Association k) . f . fmap associationValue

foldM :: (a -> Association k v -> STM a) -> a -> Map k v -> STM a
foldM = inline HAMT.foldM

toList :: Map k v -> STM [Association k v]
toList = foldM ((return .) . flip (:)) []

new :: STM (Map k v)
new = inline HAMT.new
