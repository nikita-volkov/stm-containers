module STMContainers.Map
(
  Map,
  Indexable,
  Association(..),
  new,
  insert,
  delete,
  lookup,
  focus,
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

-- |
-- Look up an item.
{-# INLINE lookup #-}
lookup :: (Indexable k) => k -> Map k v -> STM (Maybe v)
lookup k = focus Focus.lookupM k

-- |
-- Insert a key and a value.
{-# INLINE insert #-}
insert :: (Indexable k) => k -> v -> Map k v -> STM ()
insert k v = HAMT.insert (Association k v)

-- |
-- Delete an item by a key.
{-# INLINE delete #-}
delete :: (Indexable k) => k -> Map k v -> STM ()
delete = HAMT.focus Focus.deleteM

-- |
-- Focus on an item by a key with a strategy.
-- 
-- This function allows to perform composite operations in a single access
-- to a map item.
-- E.g., you can lookup an item and delete it at the same time,
-- or update it and return the new value.
{-# INLINE focus #-}
focus :: (Indexable k) => Focus.StrategyM STM v r -> k -> Map k v -> STM r
focus f k = HAMT.focus f' k
  where
    f' = (fmap . fmap . fmap) (Association k) . f . fmap associationValue

-- |
-- Fold all the items of a map.
{-# INLINE foldM #-}
foldM :: (a -> Association k v -> STM a) -> a -> Map k v -> STM a
foldM = HAMT.foldM

-- |
-- Construct a new map.
{-# INLINE new #-}
new :: STM (Map k v)
new = HAMT.new

-- |
-- Check, whether the map is empty.
{-# INLINE null #-}
null :: Map k v -> STM Bool
null = HAMT.null
