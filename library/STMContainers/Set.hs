module STMContainers.Set
(
  Set,
  Element,
  new,
  insert,
  delete,
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
-- A hash set, based on an STM-specialized hash array mapped trie.
newtype Set e = Set {hamt :: HAMT.HAMT (HAMTElement e)}

-- |
-- A standard constraint for elements.
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
-- Fold all the elements.
{-# INLINE foldM #-}
foldM :: (a -> e -> STM a) -> a -> Set e -> STM a
foldM f a = HAMT.foldM (\a -> f a . elementValue) a . hamt

-- |
-- Construct a new set.
{-# INLINE new #-}
new :: STM (Set e)
new = Set <$> HAMT.new

-- |
-- Check, whether the set is empty.
{-# INLINE null #-}
null :: Set e -> STM Bool
null = HAMT.null . hamt
