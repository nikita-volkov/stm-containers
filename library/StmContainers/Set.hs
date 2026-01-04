module StmContainers.Set
  ( Set,
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
  )
where

import qualified Focus as B
import StmContainers.Prelude hiding (delete, empty, foldM, insert, lookup, null, toList)
import qualified StmHamt.SizedHamt as A

-- |
-- A hash set, based on an STM-specialized hash array mapped trie.
newtype Set item
  = Set (A.SizedHamt item)

-- |
-- Construct a new set.
{-# INLINEABLE new #-}
new :: STM (Set item)
new =
  Set <$> A.new

-- |
-- Construct a new set in IO.
--
-- This is useful for creating it on a top-level using 'unsafePerformIO',
-- because using 'atomically' inside 'unsafePerformIO' isn't possible.
{-# INLINEABLE newIO #-}
newIO :: IO (Set item)
newIO =
  Set <$> A.newIO

-- |
-- Check, whether the set is empty.
{-# INLINEABLE null #-}
null :: Set item -> STM Bool
null (Set hamt) =
  A.null hamt

-- |
-- Get the number of elements.
{-# INLINEABLE size #-}
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
{-# INLINEABLE focus #-}
focus :: (Hashable item) => B.Focus () STM result -> item -> Set item -> STM result
focus unitFocus item (Set hamt) =
  A.focus rowFocus id item hamt
  where
    rowFocus =
      B.mappingInput (const item) (const ()) unitFocus

-- |
-- Lookup an element.
{-# INLINEABLE lookup #-}
lookup :: (Hashable item) => item -> Set item -> STM Bool
lookup =
  focus (fmap isJust B.lookup)

-- |
-- Insert a new element.
{-# INLINEABLE insert #-}
insert :: (Hashable item) => item -> Set item -> STM ()
insert item (Set hamt) =
  A.insert id item hamt

-- |
-- Delete an element.
{-# INLINEABLE delete #-}
delete :: (Hashable item) => item -> Set item -> STM ()
delete item (Set hamt) =
  A.focus B.delete id item hamt

-- |
-- Delete all the elements.
{-# INLINEABLE reset #-}
reset :: Set item -> STM ()
reset (Set hamt) =
  A.reset hamt

-- |
-- Stream the elements actively.
--
-- Amongst other features this function provides an interface to folding.
{-# INLINEABLE unfoldlM #-}
unfoldlM :: Set item -> UnfoldlM STM item
unfoldlM (Set hamt) =
  A.unfoldlM hamt

-- |
-- Stream the elements passively.
{-# INLINE listT #-}
listT :: Set item -> ListT STM item
listT (Set hamt) =
  A.listT hamt
