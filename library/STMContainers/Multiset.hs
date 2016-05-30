module STMContainers.Multiset
(
  Multiset,
  Set.Element,
  new,
  newIO,
  insert,
  insertMany,
  delete,
  deleteMany,
  deleteAll,
  count,
  focus,
  null,
  size,
  distinctSize,
  stream,
)
where

import qualified Focus
import qualified ListT
import STMContainers.Prelude hiding (insert, delete, null)
import qualified STMContainers.Map as Map
import qualified STMContainers.Set as Set


-- |
-- A multiset (also called "bag"), based on an STM-specialized hash array
-- mapped trie.
--
-- This is just a thin wrapper around @'Map.Map' e 'Int'@.
--
-- The number of occurrences for each element in the multiset is always strictly
-- positive.
newtype Multiset e = Multiset (Map.Map e Int)
  deriving (Typeable)

-- |
-- Focus on an element with a strategy.
--
-- This function allows to perform simultaneous lookup and modification.
--
-- A modification that would result in a non-positive number of occurrences deletes
-- the element instead.
{-# INLINE focus #-}
focus :: Set.Element e => Focus.StrategyM STM Int r -> e -> Multiset e -> STM r
focus f e (Multiset m) = Map.focus f' e m
  where
    f' moccurs = do
      (r, decision) <- f moccurs
      case decision of
        Focus.Replace n | n <= 0 ->
          return (r, Focus.Remove)
        _ -> return (r, decision)

{-# INLINE addOccurrences #-}
addOccurrences :: Set.Element e => e -> Int -> Multiset e -> STM ()
addOccurrences e n s
  = case n of
      0 -> return ()
      _ -> focus (Focus.alterM (return . f)) e s
  where
    f (Just occurs) = Just (occurs + n)
    f Nothing       = Just n

-- |
-- Construct a new multiset.
{-# INLINE new #-}
new :: STM (Multiset e)
new = Multiset <$> Map.new

-- |
-- Construct a new multiset in IO.
--
-- This is useful for creating it on a top-level using 'unsafePerformIO',
-- because using 'atomically' inside 'unsafePerformIO' isn't possible.
{-# INLINE newIO #-}
newIO :: IO (Multiset e)
newIO = Multiset <$> Map.newIO

-- |
-- Insert an element.
{-# INLINE insert #-}
insert :: Set.Element e => e -> Multiset e -> STM ()
insert e s = insertMany e 1 s

-- |
-- Insert several occurrences of an element.
--
-- A negative number removes elements instead.
{-# INLINE insertMany #-}
insertMany :: Set.Element e => e -> Int -> Multiset e -> STM ()
insertMany = addOccurrences

-- |
-- Delete a single occurrence of an element.
{-# INLINE delete #-}
delete :: Set.Element e => e -> Multiset e -> STM ()
delete e = deleteMany e 1

-- |
-- Delete several occurrences of an element.
--
-- A negative number will insert elements instead.
{-# INLINE deleteMany #-}
deleteMany :: Set.Element e => e -> Int -> Multiset e -> STM ()
deleteMany e n = addOccurrences e (- n)

-- |
-- Delete all occurrences of an element.
{-# INLINE deleteAll #-}
deleteAll :: Set.Element e => e -> Multiset e -> STM ()
deleteAll e (Multiset m) = Map.delete e m

-- |
-- Get the number of occurrences for an element.
{-# INLINE count #-}
count :: Set.Element e => e -> Multiset e -> STM Int
count e (Multiset m) = fromMaybe 0 <$> Map.lookup e m

-- |
-- Check whether the multiset is empty.
{-# INLINE null #-}
null :: Multiset e -> STM Bool
null (Multiset m) = Map.null m

-- |
-- Get the total number of elements.
{-# INLINE size #-}
size :: Multiset e -> STM Int
size = ListT.fold (\acc (_, o) -> return $! acc + o) 0 . stream

-- |
-- Get the number of distinct elements.
{-# INLINE distinctSize #-}
distinctSize :: Multiset e -> STM Int
distinctSize (Multiset m) = Map.size m

-- |
-- Stream the distinct elements paired with their respective number of
-- ocurrences.
{-# INLINE stream #-}
stream :: Multiset e -> ListT STM (e, Int)
stream (Multiset m) = Map.stream m
