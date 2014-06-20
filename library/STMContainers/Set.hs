module STMContainers.Set
(
  Set,
  Indexable,
  new,
  insert,
  delete,
  lookup,
  foldM,
)
where

import STMContainers.Prelude hiding (insert, delete, lookup, alter, foldM, toList, empty)
import qualified STMContainers.HAMT as HAMT
import qualified STMContainers.HAMT.Node as HAMTNode
import qualified Focus


-- |
-- A hash set, based on an STM-specialized hash array mapped trie.
type Set e = HAMT.HAMT (Element e)

-- |
-- A standard constraint for elements.
type Indexable a = (Eq a, Hashable a)

newtype Element e = Element e

instance (Eq e) => HAMTNode.Element (Element e) where
  type ElementIndex (Element e) = e
  elementIndex (Element e) = e

elementValue :: Element e -> e
elementValue (Element e) = e

insert :: (Indexable e) => e -> Set e -> STM ()
insert e = inline HAMT.insert (Element e)

delete :: (Indexable e) => e -> Set e -> STM ()
delete = inline HAMT.delete

lookup :: (Indexable e) => e -> Set e -> STM Bool
lookup k = fmap (maybe False (const True)) . inline HAMT.lookup k

foldM :: (a -> e -> STM a) -> a -> Set e -> STM a
foldM f = inline HAMT.foldM (\a -> f a . elementValue)

new :: STM (Set e)
new = inline HAMT.new
