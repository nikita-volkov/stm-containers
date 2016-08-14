module STM.Containers.Multimap
(
  Multimap,
  new,
  newIO,
  null,
  focus,
  lookup,
  lookupByKey,
  insert,
  delete,
  deleteByKey,
  deleteAll,
  stream,
  streamKeys,
  streamByKey,
)
where

import STM.Containers.Private.Prelude hiding (insert, delete, lookup, alter, foldM, toList, empty, null)
import qualified STM.Containers.Map as A
import qualified STM.Containers.Set as B
import qualified STM.Containers.Private.Focuses as D
import qualified Focus.Impure as C


-- |
-- A multimap, based on an STM-specialized hash array mapped trie.
--
-- Basically it's just a wrapper API around @'A.Map' key ('B.Set' value)@.
newtype Multimap key value =
  Multimap (A.Map key (B.Set value))
  deriving (Typeable)

-- |
-- Construct a new multimap.
{-# INLINE new #-}
new :: STM (Multimap key value)
new =
  Multimap <$> A.new

-- |
-- Construct a new multimap in IO.
--
-- This is useful for creating it on a top-level using 'unsafePerformIO',
-- because using 'atomically' inside 'unsafePerformIO' isn't possible.
{-# INLINE newIO #-}
newIO :: IO (Multimap key value)
newIO =
  Multimap <$> A.newIO

-- |
-- Check on being empty.
{-# INLINE null #-}
null :: Multimap key value -> STM Bool
null (Multimap map) =
  A.null map

-- |
-- Focus on an item by the value and the key.
--
-- This function allows to perform simultaneous lookup and modification.
--
-- The focus is over a unit since we already know,
-- which value we're focusing on and it doesn't make sense to replace it,
-- however we still can decide wether to keep or remove it.
{-# INLINE focus #-}
focus :: (Eq key, Hashable key, Eq value, Hashable value) => C.Focus () STM result -> value -> key -> Multimap key value -> STM result
focus unitFocus value key (Multimap map) =
  A.focus setFocus key map
  where
    setFocus =
      \case
        Nothing ->
          do
            (output, instruction) <- unitFocus Nothing
            case instruction of
              C.Set () ->
                do
                  set <- B.new
                  B.insert value set
                  return (output, C.Set set)
              _ ->
                return (output, C.Keep)
        Just set ->
          do
            output <- B.focus unitFocus value set
            instruction <- bool C.Keep C.Remove <$> B.null set
            return (output, instruction)

-- |
-- Look up an item by a value and a key.
{-# INLINE lookup #-}
lookup :: (Eq key, Hashable key, Eq value, Hashable value) => value -> key -> Multimap key value -> STM Bool
lookup value key (Multimap m) =
  maybe (return False) (B.lookup value) =<< A.lookup key m

-- |
-- Look up all values by key.
{-# INLINE lookupByKey #-}
lookupByKey :: (Eq key, Hashable key) => key -> Multimap key value -> STM (Maybe (B.Set value))
lookupByKey key (Multimap m) =
  A.lookup key m

-- |
-- Insert an item.
{-# INLINABLE insert #-}
insert :: (Eq key, Hashable key, Eq value, Hashable value) => value -> key -> Multimap key value -> STM ()
insert value key (Multimap map) =
  A.focus setFocus key map
  where
    setFocus =
      \case
        Just set ->
          do
            B.insert value set
            return ((), C.Keep)
        Nothing ->
          do
            set <- B.new
            B.insert value set
            return ((), C.Set set)

-- |
-- Delete an item by a value and a key.
{-# INLINABLE delete #-}
delete :: (Eq key, Hashable key, Eq value, Hashable value) => value -> key -> Multimap key value -> STM ()
delete value key (Multimap map) =
  A.focus setFocus key map
  where
    setFocus =
      \case
        Just set ->
          do
            B.delete value set
            B.null set >>= returnDecision . bool C.Keep C.Remove
        Nothing ->
          returnDecision C.Keep
      where
        returnDecision c = return ((), c)

-- |
-- Delete all values associated with the key.
{-# INLINEABLE deleteByKey #-}
deleteByKey :: (Eq key, Hashable key) => key -> Multimap key value -> STM ()
deleteByKey key (Multimap map) =
  A.delete key map

-- |
-- Delete all the associations.
{-# INLINE deleteAll #-}
deleteAll :: Multimap key value -> STM ()
deleteAll (Multimap map) =
  A.deleteAll map

-- |
-- Stream associations.
--
-- Amongst other features this function provides an interface to folding
-- via the 'ListT.fold' function.
stream :: Multimap key value -> ListT STM (key, value)
stream (Multimap m) =
  A.stream m >>= \(key, s) -> (key,) <$> B.stream s

-- |
-- Stream keys.
streamKeys :: Multimap key value -> ListT STM key
streamKeys (Multimap m) =
  fmap fst (A.stream m)

-- |
-- Stream values by a key.
streamByKey :: (Eq key, Hashable key) => key -> Multimap key value -> ListT STM value
streamByKey key (Multimap m) =
  lift (A.lookup key m) >>= maybe mempty B.stream


