module STMContainers.Multimap
(
  Multimap,
  Association,
  new,
  insert,
  delete,
  lookup,
  foldM,
  null,
)
where

import STMContainers.Prelude hiding (insert, delete, lookup, alter, foldM, toList, empty, null)
import qualified Focus
import qualified STMContainers.Map as Map
import qualified STMContainers.Set as Set


-- |
-- A multimap, based on an STM-specialized hash array mapped trie.
-- 
-- Basically it's just a wrapper API around @'Map.Map' k ('Set.Set' v)@.
newtype Multimap k v = Multimap (Map.Map k (Set.Set v))

-- |
-- A standard constraint for items.
type Association k v = (Eq k, Hashable k, Eq v, Hashable v)

-- |
-- Look up an item by a value and a key.
{-# INLINE lookup #-}
lookup :: (Association k v) => v -> k -> Multimap k v -> STM Bool
lookup v k (Multimap m) = 
  maybe (return False) (Set.lookup v) =<< Map.lookup k m

-- |
-- Insert an item.
{-# INLINABLE insert #-}
insert :: (Association k v) => v -> k -> Multimap k v -> STM ()
insert v k (Multimap m) =
  Map.focus ms k m
  where
    ms = 
      \case 
        Just s -> 
          do
            Set.insert v s
            return ((), Focus.Keep)
        Nothing ->
          do
            s <- Set.new
            Set.insert v s
            return ((), Focus.Replace s)

-- |
-- Delete an item by a value and a key.
{-# INLINABLE delete #-}
delete :: (Association k v) => v -> k -> Multimap k v -> STM ()
delete v k (Multimap m) =
  Map.focus ms k m
  where
    ms = 
      \case 
        Just s -> 
          do
            Set.delete v s
            Set.null s >>= returnDecision . bool Focus.Keep Focus.Remove
        Nothing ->
          returnDecision Focus.Keep
      where
        returnDecision c = return ((), c)

-- |
-- Fold all the items.
{-# INLINE foldM #-}
foldM :: (a -> (k, v) -> STM a) -> a -> Multimap k v -> STM a
foldM f a (Multimap m) = 
  Map.foldM f' a m
  where
    f' a' (k, set) = 
      Set.foldM f'' a' set
      where
        f'' a'' v = f a'' (k, v)

-- |
-- Construct a new multimap.
{-# INLINE new #-}
new :: STM (Multimap k v)
new = Multimap <$> Map.new

-- |
-- Check on being empty.
{-# INLINE null #-}
null :: Multimap k v -> STM Bool
null (Multimap m) = Map.null m
