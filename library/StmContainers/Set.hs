module StmContainers.Set
(
  Set,
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
-- A hash set, based on an STM-specialized hash array mapped trie.
newtype Set item =
  Set (A.SizedHamt item)
  deriving (Typeable)

-- |
-- Construct a new set.
{-# INLINABLE new #-}
new :: STM (Set item)
new =
  Set <$> A.new

-- |
-- Construct a new set in IO.
-- 
-- This is useful for creating it on a top-level using 'unsafePerformIO', 
-- because using 'atomically' inside 'unsafePerformIO' isn't possible.
{-# INLINABLE newIO #-}
newIO :: IO (Set item)
newIO =
  Set <$> A.newIO

-- |
-- Check, whether the set is empty.
{-# INLINABLE null #-}
null :: Set item -> STM Bool
null (Set hamt) =
  A.null hamt

-- |
-- Get the number of elements.
{-# INLINABLE size #-}
size :: Set item -> STM Int
size (Set hamt) =
  A.size hamt

-- |
-- Focus on an element with a strategy.
-- 
-- This function allows to perform simultaneous lookup and modification.
-- 
-- The strategy is over a unit since we already know, 
-- which element we're focusing on and it doesn't make sense to replace it,
-- however we still can decide wether to keep or remove it.
{-# INLINABLE focus #-}
focus :: (Eq item, Hashable item) => B.Focus () STM result -> item -> Set item -> STM result
focus unitFocus item (Set hamt) =
  A.focus rowFocus id item hamt
  where
    rowFocus = 
      B.mappingInput (const item) (const ()) unitFocus

-- |
-- Lookup an element.
{-# INLINABLE lookup #-}
lookup :: (Eq item, Hashable item) => item -> Set item -> STM Bool
lookup =
  focus (fmap isJust B.lookup)

-- |
-- Insert a new element.
{-# INLINABLE insert #-}
insert :: (Eq item, Hashable item) => item -> Set item -> STM ()
insert item (Set hamt) =
  A.insert id item hamt

-- |
-- Delete an element.
{-# INLINABLE delete #-}
delete :: (Eq item, Hashable item) => item -> Set item -> STM ()
delete item (Set hamt) =
  A.focus B.delete id item hamt

-- |
-- Delete all the elements.
{-# INLINABLE reset #-}
reset :: Set item -> STM ()
reset (Set hamt) =
  A.reset hamt

-- |
-- Stream elements.
-- 
-- Amongst other features this function provides an interface to folding.
{-# INLINABLE unfoldM #-}
unfoldM :: Set item -> UnfoldM STM item
unfoldM (Set hamt) =
  A.unfoldM hamt
