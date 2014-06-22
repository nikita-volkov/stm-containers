module STMContainers.Map
(
  Map,
  Indexable,
  Association(..),
  new,
  insert,
  delete,
  focus,
  lookup,
  foldM,
  null,
)
where

import STMContainers.Prelude hiding (insert, delete, lookup, alter, foldM, toList, empty, null)
import qualified STMContainers.HAMT as HAMT
import qualified STMContainers.HAMT.Node as HAMTNode
import qualified Focus


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

lookup :: (Indexable k) => k -> Map k v -> STM (Maybe v)
lookup k = inline focus Focus.lookupM k

insert :: (Indexable k) => k -> v -> Map k v -> STM ()
insert k v = inline focus (Focus.insertM v) k

delete :: (Indexable k) => k -> Map k v -> STM ()
delete = inline HAMT.focus Focus.deleteM

focus :: (Indexable k) => (Focus.StrategyM STM v r) -> k -> Map k v -> STM r
focus f k = inline HAMT.focus f' k
  where
    f' = (fmap . fmap . fmap) (Association k) . f . fmap associationValue

foldM :: (a -> Association k v -> STM a) -> a -> Map k v -> STM a
foldM = inline HAMT.foldM

new :: STM (Map k v)
new = inline HAMT.new

null :: Map k v -> STM Bool
null = inline HAMT.null
