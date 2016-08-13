module STM.Containers.Bimap
(
  Bimap,
  new,
  newIO,
  null,
  size,
  focus1,
  focus2,
  lookup1,
  lookup2,
  insert1,
  insert2,
  delete1,
  delete2,
  deleteAll,
  stream,
)
where

import STM.Containers.Private.Prelude hiding (insert, delete, lookup, alter, foldM, toList, empty, null)
import qualified STM.Containers.Map as A
import qualified STM.Containers.Private.Focuses as C
import qualified Focus.Impure as B


-- |
-- Bidirectional map.
-- Essentially, a bijection between subsets of its two argument types.
-- 
-- For one value of the left-hand type this map contains one value 
-- of the right-hand type and vice versa.
data Bimap key1 key2 = 
  Bimap !(A.Map key1 key2) !(A.Map key2 key1)
  deriving (Typeable)

-- |
-- Construct a new bimap.
{-# INLINE new #-}
new :: STM (Bimap key1 key2)
new =
  Bimap <$> A.new <*> A.new

-- |
-- Construct a new bimap in IO.
-- 
-- This is useful for creating it on a top-level using 'unsafePerformIO', 
-- because using 'atomically' inside 'unsafePerformIO' isn't possible.
{-# INLINE newIO #-}
newIO :: IO (Bimap key1 key2)
newIO =
  Bimap <$> A.newIO <*> A.newIO

-- |
-- Check on being empty.
{-# INLINE null #-}
null :: Bimap key1 key2 -> STM Bool
null (Bimap map1 _) =
  A.null map1

-- |
-- Get the number of elements.
{-# INLINE size #-}
size :: Bimap key1 key2 -> STM Int
size (Bimap map1 _) =
  A.size map1

-- |
-- Focus on a right value by the left value.
-- 
-- This function allows to perform composite operations in a single access
-- to a map item.
-- E.g., you can look up an item and delete it at the same time,
-- or update it and return the new value.
{-# INLINE focus1 #-}
focus1 :: (Eq key1, Hashable key1, Eq key2, Hashable key2) => B.Focus key2 STM result -> key1 -> Bimap key1 key2 -> STM result
focus1 valueFocus1 key1 (Bimap map1 map2) =
  do 
    ((output, instruction), maybeKey2) <- A.focus (C.detalising valueFocus1) key1 map1
    case instruction of
      B.Keep -> 
        return ()
      B.Remove -> 
        forM_ maybeKey2 $ \key2 -> A.delete key2 map2
      B.Set newKey2 ->
        do
          forM_ maybeKey2 $ \key2 -> A.delete key2 map2
          A.insert key1 newKey2 map2
    return output

-- |
-- Focus on a left value by the right value.
-- 
-- This function allows to perform composite operations in a single access
-- to a map item.
-- E.g., you can look up an item and delete it at the same time,
-- or update it and return the new value.
{-# INLINE focus2 #-}
focus2 :: (Eq key1, Hashable key1, Eq key2, Hashable key2) => B.Focus key1 STM result -> key2 -> Bimap key1 key2 -> STM result
focus2 valueFocus2 key2 (Bimap map1 map2) =
  focus1 valueFocus2 key2 (Bimap map2 map1)

-- |
-- Look up a right value by the left value.
{-# INLINE lookup1 #-}
lookup1 :: (Eq key1, Hashable key1, Eq key2, Hashable key2) => key1 -> Bimap key1 key2 -> STM (Maybe key2)
lookup1 key1 (Bimap map1 _) =
  A.lookup key1 map1

-- |
-- Look up a left value by the right value.
{-# INLINE lookup2 #-}
lookup2 :: (Eq key1, Hashable key1, Eq key2, Hashable key2) => key2 -> Bimap key1 key2 -> STM (Maybe key1)
lookup2 key2 (Bimap _ map2) =
  A.lookup key2 map2

-- |
-- Insert the association by the left value.
{-# INLINE insert1 #-}
insert1 :: (Eq key1, Hashable key1, Eq key2, Hashable key2) => key2 -> key1 -> Bimap key1 key2 -> STM ()
insert1 key2 key1 (Bimap map1 map2) = 
  do
    A.insert key2 key1 map1
    A.insert key1 key2 map2

-- |
-- Insert the association by the right value.
{-# INLINE insert2 #-}
insert2 :: (Eq key1, Hashable key1, Eq key2, Hashable key2) => key1 -> key2 -> Bimap key1 key2 -> STM ()
insert2 key1 key2 (Bimap map1 map2) = 
  do
    A.insert key1 key2 map2
    A.insert key2 key1 map1

-- |
-- Delete the association by the left value.
{-# INLINE delete1 #-}
delete1 :: (Eq key1, Hashable key1, Eq key2, Hashable key2) => key1 -> Bimap key1 key2 -> STM ()
delete1 key1 (Bimap map1 map2) =
  A.focus B.lookupAndDelete key1 map1 >>= 
  mapM_ (\key2 -> A.delete key2 map2)
  
-- |
-- Delete the association by the right value.
{-# INLINE delete2 #-}
delete2 :: (Eq key1, Hashable key1, Eq key2, Hashable key2) => key2 -> Bimap key1 key2 -> STM ()
delete2 key2 (Bimap map1 map2) =
  delete1 key2 (Bimap map2 map1)

-- |
-- Delete all the associations.
{-# INLINE deleteAll #-}
deleteAll :: Bimap key1 key2 -> STM ()
deleteAll (Bimap map1 map2) =
  do
    A.deleteAll map1
    A.deleteAll map2

-- |
-- Stream associations.
-- 
-- Amongst other features this function provides an interface to folding 
-- via the 'ListT.fold' function.
{-# INLINE stream #-}
stream :: Bimap key1 key2 -> ListT STM (key1, key2)
stream (Bimap map1 map2) =
  A.stream map1
