module STMContainers.Map
(
  Map,
  Key,
  new,
  newIO,
  insert,
  delete,
  lookup,
  focus,
  null,
  stream,
)
where

import STMContainers.Prelude hiding (insert, delete, lookup, alter, foldM, toList, empty, null)
import qualified STMContainers.HAMT as HAMT
import qualified STMContainers.HAMT.Nodes as HAMTNodes
import qualified Focus


-- |
-- A hash table, based on an STM-specialized hash array mapped trie.
newtype Map k v = Map (HAMT.HAMT (Association k v))
  deriving (Typeable)

-- |
-- A standard constraint for keys.
type Key a = (Eq a, Hashable a)

-- |
-- A key-value association.
type Association k v = (k, v)

instance (Eq k) => HAMTNodes.Element (Association k v) where
  type ElementKey (Association k v) = k
  elementKey (k, v) = k

{-# INLINE associationValue #-}
associationValue :: Association k v -> v
associationValue (_, v) = v

-- |
-- Look up an item.
{-# INLINE lookup #-}
lookup :: (Key k) => k -> Map k v -> STM (Maybe v)
lookup k = focus Focus.lookupM k

-- |
-- Insert a value at a key.
{-# INLINE insert #-}
insert :: (Key k) => v -> k -> Map k v -> STM ()
insert !v !k (Map h) = HAMT.insert (k, v) h

-- |
-- Delete an item by a key.
{-# INLINE delete #-}
delete :: (Key k) => k -> Map k v -> STM ()
delete k (Map h) = HAMT.focus Focus.deleteM k h

-- |
-- Focus on an item by a key with a strategy.
-- 
-- This function allows to perform composite operations in a single access
-- to a map item.
-- E.g., you can look up an item and delete it at the same time,
-- or update it and return the new value.
{-# INLINE focus #-}
focus :: (Key k) => Focus.StrategyM STM v r -> k -> Map k v -> STM r
focus f k (Map h) = HAMT.focus f' k h
  where
    f' = (fmap . fmap . fmap) (\v -> k `seq` v `seq` (k, v)) . f . fmap associationValue

-- |
-- Construct a new map.
{-# INLINE new #-}
new :: STM (Map k v)
new = Map <$> HAMT.new

-- |
-- Construct a new map in IO.
-- 
-- This is useful for creating it on a top-level using 'unsafePerformIO', 
-- because using 'atomically' inside 'unsafePerformIO' isn't possible.
{-# INLINE newIO #-}
newIO :: IO (Map k v)
newIO = Map <$> HAMT.newIO

-- |
-- Check, whether the map is empty.
{-# INLINE null #-}
null :: Map k v -> STM Bool
null (Map h) = HAMT.null h

-- |
-- Stream associations.
-- 
-- Amongst other features this function provides an interface to folding 
-- via the 'ListT.fold' function.
{-# INLINE stream #-}
stream :: Map k v -> ListT STM (k, v)
stream (Map h) = HAMT.stream h
