module StmContainers.Multimap
  ( Multimap,
    new,
    newIO,
    null,
    focus,
    lookup,
    lookupByKey,
    insert,
    delete,
    deleteByKey,
    reset,
    unfoldlM,
    unfoldlMKeys,
    unfoldlMByKey,
    listT,
    listTKeys,
    listTByKey,
  )
where

import qualified Focus as C
import qualified StmContainers.Map as A
import StmContainers.Prelude hiding (delete, empty, foldM, insert, lookup, null, toList)
import qualified StmContainers.Set as B

-- |
-- A multimap, based on an STM-specialized hash array mapped trie.
--
-- Basically it's just a wrapper API around @'A.Map' key ('B.Set' value)@.
newtype Multimap key value
  = Multimap (A.Map key (B.Set value))

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
focus :: (Hashable key, Hashable value) => C.Focus () STM result -> value -> key -> Multimap key value -> STM result
focus unitFocus@(Focus concealUnit _) value key (Multimap map) = A.focus setFocus key map
  where
    setFocus = C.Focus conceal reveal
      where
        conceal = do
          (output, change) <- concealUnit
          case change of
            C.Set () ->
              do
                set <- B.new
                B.insert value set
                return (output, C.Set set)
            _ ->
              return (output, C.Leave)
    reveal set = do
      output <- B.focus unitFocus value set
      change <- bool C.Leave C.Remove <$> B.null set
      return (output, change)

-- |
-- Look up an item by a value and a key.
{-# INLINE lookup #-}
lookup :: (Hashable key, Hashable value) => value -> key -> Multimap key value -> STM Bool
lookup value key (Multimap m) =
  maybe (return False) (B.lookup value) =<< A.lookup key m

-- |
-- Look up all values by key.
{-# INLINE lookupByKey #-}
lookupByKey :: (Hashable key) => key -> Multimap key value -> STM (Maybe (B.Set value))
lookupByKey key (Multimap m) =
  A.lookup key m

-- |
-- Insert an item.
{-# INLINEABLE insert #-}
insert :: (Hashable key, Hashable value) => value -> key -> Multimap key value -> STM ()
insert value key (Multimap map) = A.focus setFocus key map
  where
    setFocus = Focus conceal reveal
      where
        conceal = do
          set <- B.new
          B.insert value set
          return ((), C.Set set)
        reveal set = do
          B.insert value set
          return ((), C.Leave)

-- |
-- Delete an item by a value and a key.
{-# INLINEABLE delete #-}
delete :: (Hashable key, Hashable value) => value -> key -> Multimap key value -> STM ()
delete value key (Multimap map) = A.focus setFocus key map
  where
    setFocus = Focus conceal reveal
      where
        conceal = returnChange C.Leave
        reveal set = do
          B.delete value set
          B.null set >>= returnChange . bool C.Leave C.Remove
        returnChange c = return ((), c)

-- |
-- Delete all values associated with the key.
{-# INLINEABLE deleteByKey #-}
deleteByKey :: (Hashable key) => key -> Multimap key value -> STM ()
deleteByKey key (Multimap map) =
  A.delete key map

-- |
-- Delete all the associations.
{-# INLINE reset #-}
reset :: Multimap key value -> STM ()
reset (Multimap map) =
  A.reset map

-- |
-- Stream associations actively.
--
-- Amongst other features this function provides an interface to folding.
unfoldlM :: Multimap key value -> UnfoldlM STM (key, value)
unfoldlM (Multimap m) =
  A.unfoldlM m >>= \(key, s) -> (key,) <$> B.unfoldlM s

-- |
-- Stream keys actively.
unfoldlMKeys :: Multimap key value -> UnfoldlM STM key
unfoldlMKeys (Multimap m) =
  fmap fst (A.unfoldlM m)

-- |
-- Stream values by a key actively.
unfoldlMByKey :: (Hashable key) => key -> Multimap key value -> UnfoldlM STM value
unfoldlMByKey key (Multimap m) =
  lift (A.lookup key m) >>= maybe mempty B.unfoldlM

-- |
-- Stream associations passively.
listT :: Multimap key value -> ListT STM (key, value)
listT (Multimap m) =
  A.listT m >>= \(key, s) -> (key,) <$> B.listT s

-- |
-- Stream keys passively.
listTKeys :: Multimap key value -> ListT STM key
listTKeys (Multimap m) =
  fmap fst (A.listT m)

-- |
-- Stream values by a key passively.
listTByKey :: (Hashable key) => key -> Multimap key value -> ListT STM value
listTByKey key (Multimap m) =
  lift (A.lookup key m) >>= maybe mempty B.listT
