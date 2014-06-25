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
import qualified STMContainers.HAMT.Nodes as HAMTNodes
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

instance (Eq k) => HAMTNodes.Element (Association k v) where
  type ElementKey (Association k v) = k
  elementKey (Association k v) = k

{-# INLINE associationValue #-}
associationValue :: Association k v -> v
associationValue (Association _ v) = v

{-# INLINE lookup #-}
lookup :: (Indexable k) => k -> Map k v -> STM (Maybe v)
lookup k = focus Focus.lookupM k

{-# INLINE insert #-}
insert :: (Indexable k) => k -> v -> Map k v -> STM ()
insert k v = HAMT.insert (Association k v)

{-# INLINE delete #-}
delete :: (Indexable k) => k -> Map k v -> STM ()
delete = HAMT.focus Focus.deleteM

{-# INLINE focus #-}
focus :: (Indexable k) => (Focus.StrategyM STM v r) -> k -> Map k v -> STM r
focus f k = HAMT.focus f' k
  where
    f' = (fmap . fmap . fmap) (Association k) . f . fmap associationValue

{-# INLINE foldM #-}
foldM :: (a -> Association k v -> STM a) -> a -> Map k v -> STM a
foldM = HAMT.foldM

{-# INLINE new #-}
new :: STM (Map k v)
new = HAMT.new

{-# INLINE null #-}
null :: Map k v -> STM Bool
null = HAMT.null
