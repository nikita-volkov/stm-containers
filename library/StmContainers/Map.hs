module StmContainers.Map
  ( Map,
    new,
    newIO,
    null,
    size,
    focus,
    lookup,
    insert,
    delete,
    reset,
    unfoldlM,
    listT,
    listTNonAtomic,
  )
where

import qualified DeferredFolds.UnfoldlM as C
import qualified Focus as B
import StmContainers.Prelude hiding (delete, empty, foldM, insert, lookup, null, toList)
import qualified StmHamt.Hamt as A

-- |
-- Hash-table, based on STM-specialized Hash Array Mapped Trie.
newtype Map key value
  = Map (A.Hamt (Product2 key value))

-- |
-- Construct a new map.
{-# INLINEABLE new #-}
new :: STM (Map key value)
new =
  Map <$> A.new

-- |
-- Construct a new map in IO.
--
-- This is useful for creating it on a top-level using 'unsafePerformIO',
-- because using 'atomically' inside 'unsafePerformIO' isn't possible.
{-# INLINEABLE newIO #-}
newIO :: IO (Map key value)
newIO =
  Map <$> A.newIO

-- |
-- Check, whether the map is empty.
{-# INLINEABLE null #-}
null :: Map key value -> STM Bool
null (Map hamt) =
  A.null hamt

-- |
-- Get the number of elements.
{-# INLINEABLE size #-}
size :: Map key value -> STM Int
size =
  C.foldlM' (\x _ -> return (succ x)) 0 . unfoldlM

-- |
-- Focus on a value by the key.
--
-- This function allows to perform composite operations in a single access
-- to the map's row.
-- E.g., you can look up a value and delete it at the same time,
-- or update it and return the new value.
{-# INLINE focus #-}
focus :: (Hashable key) => B.Focus value STM result -> key -> Map key value -> STM result
focus valueFocus key (Map hamt) =
  A.focus rowFocus (\(Product2 key _) -> key) key hamt
  where
    rowFocus =
      B.mappingInput (\value -> Product2 key value) (\(Product2 _ value) -> value) valueFocus

-- |
-- Look up an item.
{-# INLINEABLE lookup #-}
lookup :: (Hashable key) => key -> Map key value -> STM (Maybe value)
lookup key =
  focus B.lookup key

-- |
-- Insert a value at a key.
{-# INLINE insert #-}
insert :: (Hashable key) => value -> key -> Map key value -> STM ()
insert value key (Map hamt) =
  void (A.insert (\(Product2 key _) -> key) (Product2 key value) hamt)

-- |
-- Delete an item by a key.
{-# INLINEABLE delete #-}
delete :: (Hashable key) => key -> Map key value -> STM ()
delete key =
  focus B.delete key

-- |
-- Delete all the associations.
{-# INLINEABLE reset #-}
reset :: Map key value -> STM ()
reset (Map hamt) =
  A.reset hamt

-- |
-- Stream the associations actively.
--
-- Amongst other features this function provides an interface to folding.
{-# INLINEABLE unfoldlM #-}
unfoldlM :: Map key value -> UnfoldlM STM (key, value)
unfoldlM (Map hamt) =
  fmap (\(Product2 k v) -> (k, v)) (A.unfoldlM hamt)

-- |
-- Stream the associations passively.
{-# INLINE listT #-}
listT :: Map key value -> ListT STM (key, value)
listT (Map hamt) = fmap (\(Product2 k v) -> (k, v)) (A.listT hamt)

-- |
-- Stream the associations passively.
-- Data may be inconsistent/out of date.
{-# INLINE listTNonAtomic #-}
listTNonAtomic :: Map key value -> ListT IO (key, value)
listTNonAtomic (Map hamt) = fmap (\(Product2 k v) -> (k, v)) (A.listTNonAtomic hamt)
