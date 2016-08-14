module STM.Containers.Map
(
  Map,
  new,
  newIO,
  null,
  size,
  focus,
  lookup,
  insert,
  delete,
  deleteAll,
  stream,
)
where

import STM.Containers.Private.Prelude hiding (insert, delete, lookup, alter, foldM, toList, empty, null)
import qualified STM.HAMT.Simple as A
import qualified STM.Containers.Private.Focuses as C
import qualified Focus.Impure as B


-- |
-- Hash-table, based on STM-specialized Hash Array Mapped Trie.
newtype Map key value =
  Map (A.HAMT (Row key value))
  deriving (Typeable)

data Row key value =
  Row !key value

instance Eq key => Eq (Row key value) where
  {-# INLINE (==) #-}
  Row k1 _ == Row k2 _ =
    k1 == k2

instance Hashable key => Hashable (Row key value) where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (Row key _) =
    hashWithSalt salt key

-- |
-- Construct a new map.
{-# INLINABLE new #-}
new :: STM (Map key value)
new =
  Map <$> A.new

-- |
-- Construct a new map in IO.
-- 
-- This is useful for creating it on a top-level using 'unsafePerformIO', 
-- because using 'atomically' inside 'unsafePerformIO' isn't possible.
{-# INLINABLE newIO #-}
newIO :: IO (Map key value)
newIO =
  Map <$> A.newIO

-- |
-- Check, whether the map is empty.
{-# INLINABLE null #-}
null :: Map key value -> STM Bool
null (Map hamt) =
  A.null hamt

-- |
-- Get the number of elements.
{-# INLINABLE size #-}
size :: Map key value -> STM Int
size (Map hamt) =
  A.size hamt

-- |
-- Focus on a value by the key.
-- 
-- This function allows to perform composite operations in a single access
-- to the map's row.
-- E.g., you can look up a value and delete it at the same time,
-- or update it and return the new value.
{-# INLINE focus #-}
focus :: (Eq key, Hashable key) => B.Focus value STM result -> key -> Map key value -> STM result
focus valueFocus key (Map hamt) =
  A.focus rowFocus (Row key undefined) hamt
  where
    rowFocus =
      C.mapInput (\(Row _ (!value)) -> value) (\(!value) -> Row key value) valueFocus

-- |
-- Look up an item.
{-# INLINABLE lookup #-}
lookup :: (Eq key, Hashable key) => key -> Map key value -> STM (Maybe value)
lookup key =
  focus B.lookup key

-- |
-- Insert a value at a key.
{-# INLINE insert #-}
insert :: (Eq key, Hashable key) => value -> key -> Map key value -> STM ()
insert !value key (Map hamt) =
  A.insert (Row key value) hamt

-- |
-- Delete an item by a key.
{-# INLINABLE delete #-}
delete :: (Eq key, Hashable key) => key -> Map key value -> STM ()
delete key =
  focus B.delete key

-- |
-- Delete all the associations.
{-# INLINABLE deleteAll #-}
deleteAll :: Map key value -> STM ()
deleteAll (Map hamt) =
  A.deleteAll hamt

-- |
-- Stream associations.
-- 
-- Amongst other features this function provides an interface to folding 
-- via the 'ListT.fold' function.
{-# INLINABLE stream #-}
stream :: Map key value -> ListT STM (key, value)
stream (Map hamt) =
  fmap (\(Row k v) -> (k, v)) (A.stream hamt)
