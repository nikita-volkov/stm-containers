module StmContainers.Map
(
  Map,
  new,
  newIO,
  null,
  size,
  focus,
  lookup,
  insert,
  delete,
  reset,
  unfoldM,
)
where

import StmContainers.Prelude hiding (insert, delete, lookup, alter, foldM, toList, empty, null)
import qualified StmHamt.SizedHamt as A
import qualified Focus as B


-- |
-- Hash-table, based on STM-specialized Hash Array Mapped Trie.
newtype Map key value =
  Map (A.SizedHamt (Product2 key value))

-- |
-- Construct a new map.
{-# INLINABLE new #-}
new :: STM (Map key value)
new =
  Map <$> A.new

-- |
-- Construct a new map in IO.
-- 
-- This is useful for creating it on a top-level using 'unsafePerformIO', 
-- because using 'atomically' inside 'unsafePerformIO' isn't possible.
{-# INLINABLE newIO #-}
newIO :: IO (Map key value)
newIO =
  Map <$> A.newIO

-- |
-- Check, whether the map is empty.
{-# INLINABLE null #-}
null :: Map key value -> STM Bool
null (Map hamt) =
  A.null hamt

-- |
-- Get the number of elements.
{-# INLINABLE size #-}
size :: Map key value -> STM Int
size (Map hamt) =
  A.size hamt

-- |
-- Focus on a value by the key.
-- 
-- This function allows to perform composite operations in a single access
-- to the map's row.
-- E.g., you can look up a value and delete it at the same time,
-- or update it and return the new value.
{-# INLINE focus #-}
focus :: (Eq key, Hashable key) => B.Focus value STM result -> key -> Map key value -> STM result
focus valueFocus key (Map hamt) =
  A.focus rowFocus (\(Product2 key _) -> key) key hamt
  where
    rowFocus =
      B.mappingInput (\value -> Product2 key value) (\(Product2 _ value) -> value) valueFocus

-- |
-- Look up an item.
{-# INLINABLE lookup #-}
lookup :: (Eq key, Hashable key) => key -> Map key value -> STM (Maybe value)
lookup key =
  focus B.lookup key

-- |
-- Insert a value at a key.
{-# INLINE insert #-}
insert :: (Eq key, Hashable key) => value -> key -> Map key value -> STM ()
insert value key (Map hamt) =
  A.insert (\(Product2 key _) -> key) (Product2 key value) hamt

-- |
-- Delete an item by a key.
{-# INLINABLE delete #-}
delete :: (Eq key, Hashable key) => key -> Map key value -> STM ()
delete key =
  focus B.delete key

-- |
-- Delete all the associations.
{-# INLINABLE reset #-}
reset :: Map key value -> STM ()
reset (Map hamt) =
  A.reset hamt

-- |
-- Stream the associations.
-- 
-- Amongst other features this function provides an interface to folding.
{-# INLINABLE unfoldM #-}
unfoldM :: Map key value -> UnfoldM STM (key, value)
unfoldM (Map hamt) =
  fmap (\(Product2 k v) -> (k, v)) (A.unfoldM hamt)
