module STMContainers.Bimap
(
  Bimap,
  Association,
  new,
  newIO,
  insert1,
  insert2,
  delete1,
  delete2,
  lookup1,
  lookup2,
  focus1,
  focus2,
  null,
  stream,
)
where

import STMContainers.Prelude hiding (insert, delete, lookup, alter, foldM, toList, empty, null)
import qualified Focus
import qualified STMContainers.Map as Map


-- |
-- A bidirectional map.
-- Essentially a bijection between subsets of its two argument types.
-- 
-- For one value of a left-hand type this map contains one value 
-- of the right-hand type and vice versa.
data Bimap a b = 
  Bimap {m1 :: !(Map.Map a b), m2 :: !(Map.Map b a)}
  deriving (Typeable)

-- |
-- A standard constraint for associations.
type Association a b = (Map.Key a, Map.Key b)

-- |
-- Construct a new bimap.
{-# INLINABLE new #-}
new :: STM (Bimap a b)
new = Bimap <$> Map.new <*> Map.new

-- |
-- Construct a new bimap in IO.
-- 
-- This is useful for creating it on a top-level using 'unsafePerformIO', 
-- because using 'atomically' inside 'unsafePerformIO' isn't possible.
{-# INLINABLE newIO #-}
newIO :: IO (Bimap a b)
newIO = Bimap <$> Map.newIO <*> Map.newIO

-- |
-- Check on being empty.
{-# INLINABLE null #-}
null :: Bimap a b -> STM Bool
null = Map.null . m1

-- |
-- Look up a right value by a left value.
{-# INLINABLE lookup1 #-}
lookup1 :: (Association a b) => a -> Bimap a b -> STM (Maybe b)
lookup1 k = Map.lookup k . m1

-- |
-- Look up a left value by a right value.
{-# INLINABLE lookup2 #-}
lookup2 :: (Association a b) => b -> Bimap a b -> STM (Maybe a)
lookup2 k = Map.lookup k . m2

-- |
-- Insert an association by a left value.
{-# INLINABLE insert1 #-}
insert1 :: (Association a b) => b -> a -> Bimap a b -> STM ()
insert1 b a (Bimap m1 m2) = 
  do
    Map.insert b a m1
    Map.insert a b m2

-- |
-- Insert an association by a right value.
{-# INLINABLE insert2 #-}
insert2 :: (Association a b) => a -> b -> Bimap a b -> STM ()
insert2 b a (Bimap m1 m2) = (inline insert1) b a (Bimap m2 m1)

-- |
-- Delete an association by a left value.
{-# INLINABLE delete1 #-}
delete1 :: (Association a b) => a -> Bimap a b -> STM ()
delete1 k (Bimap m1 m2) =
  Map.focus lookupAndDeleteStrategy k m1 >>= 
    mapM_ (\k' -> Map.delete k' m2)
  where
    lookupAndDeleteStrategy r =
      return (r, Focus.Remove)

-- |
-- Delete an association by a right value.
{-# INLINABLE delete2 #-}
delete2 :: (Association a b) => b -> Bimap a b -> STM ()
delete2 k (Bimap m1 m2) = (inline delete1) k (Bimap m2 m1)

-- |
-- Focus on a right value by a left value with a strategy.
-- 
-- This function allows to perform composite operations in a single access
-- to a map item.
-- E.g., you can look up an item and delete it at the same time,
-- or update it and return the new value.
{-# INLINABLE focus1 #-}
focus1 :: (Association a b) => Focus.StrategyM STM b r -> a -> Bimap a b -> STM r
focus1 s a (Bimap m1 m2) =
  do 
    (r, d, mb) <- Map.focus s' a m1
    case d of
      Focus.Keep -> 
        return ()
      Focus.Remove -> 
        forM_ mb $ \b -> Map.delete b m2
      Focus.Replace b' ->
        do
          forM_ mb $ \b -> Map.delete b m2
          Map.insert a b' m2
    return r
  where
    s' = \k -> s k >>= \(r, d) -> return ((r, d, k), d)

-- |
-- Focus on a left value by a right value with a strategy.
-- 
-- This function allows to perform composite operations in a single access
-- to a map item.
-- E.g., you can look up an item and delete it at the same time,
-- or update it and return the new value.
{-# INLINABLE focus2 #-}
focus2 :: (Association a b) => Focus.StrategyM STM a r -> b -> Bimap a b -> STM r
focus2 s b (Bimap m1 m2) = (inline focus1) s b (Bimap m2 m1)

-- |
-- Stream associations.
-- 
-- Amongst other features this function provides an interface to folding 
-- via the 'ListT.fold' function.
{-# INLINE stream #-}
stream :: Bimap a b -> ListT STM (a, b)
stream = Map.stream . m1
