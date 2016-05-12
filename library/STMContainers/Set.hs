module STMContainers.Set
(
  Set,
  Element,
  new,
  newIO,
  insert,
  delete,
  lookup,
  focus,
  null,
  size,
  stream,
)
where

import STMContainers.Prelude hiding (insert, delete, lookup, alter, foldM, toList, empty, null)
import qualified ListT as ListT
import qualified STMContainers.HAMT as HAMT
import qualified STMContainers.HAMT.Nodes as HAMTNodes
import qualified Focus


-- |
-- A hash set, based on an STM-specialized hash array mapped trie.
newtype Set e = Set {hamt :: HAMT.HAMT (HAMTElement e)}
  deriving (Typeable)

-- |
-- A constraint for elements.
type Element a = (Eq a, Hashable a)

newtype HAMTElement e = HAMTElement e

instance (Eq e) => HAMTNodes.Element (HAMTElement e) where
  type ElementKey (HAMTElement e) = e
  elementKey (HAMTElement e) = e

{-# INLINE elementValue #-}
elementValue :: HAMTElement e -> e
elementValue (HAMTElement e) = e

-- |
-- Insert a new element.
{-# INLINE insert #-}
insert :: (Element e) => e -> Set e -> STM ()
insert e = HAMT.insert (HAMTElement e) . hamt

-- |
-- Delete an element.
{-# INLINE delete #-}
delete :: (Element e) => e -> Set e -> STM ()
delete e = HAMT.focus Focus.deleteM e . hamt

-- |
-- Lookup an element.
{-# INLINE lookup #-}
lookup :: (Element e) => e -> Set e -> STM Bool
lookup e = fmap (maybe False (const True)) . HAMT.focus Focus.lookupM e . hamt

-- |
-- Focus on an element with a strategy.
--
-- This function allows to perform simultaneous lookup and modification.
--
-- The strategy is over a unit since we already know,
-- which element we're focusing on and it doesn't make sense to replace it,
-- however we still can decide wether to keep or remove it.
{-# INLINE focus #-}
focus :: (Element e) => Focus.StrategyM STM () r -> e -> Set e -> STM r
focus s e = HAMT.focus elementStrategy e . hamt
  where
    elementStrategy =
      (fmap . fmap . fmap) (const (HAMTElement e)) . s . fmap (const ())

-- |
-- Construct a new set.
{-# INLINE new #-}
new :: STM (Set e)
new = Set <$> HAMT.new

-- |
-- Construct a new set in IO.
--
-- This is useful for creating it on a top-level using 'unsafePerformIO',
-- because using 'atomically' inside 'unsafePerformIO' isn't possible.
{-# INLINE newIO #-}
newIO :: IO (Set e)
newIO = Set <$> HAMT.newIO

-- |
-- Check, whether the set is empty.
{-# INLINE null #-}
null :: Set e -> STM Bool
null = HAMT.null . hamt

-- |
-- /O(n)/. The number of elements in the set.
--
-- @
-- size xs = ListT.'ListT.fold' (\\x _ -> 'pure' (x+1)) ('stream' xs)
-- @
{-# INLINE size #-}
size :: Set e -> STM Int
size = ListT.fold (\x _ -> let y = x+1 in y `seq` pure y) 0 . stream

-- |
-- Stream elements.
--
-- Amongst other features this function provides an interface to folding
-- via the 'ListT.fold' function.
{-# INLINE stream #-}
stream :: Set e -> ListT STM e
stream = fmap elementValue . HAMT.stream . hamt
