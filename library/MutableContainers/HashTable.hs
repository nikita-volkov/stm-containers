module MutableContainers.HashTable where

import MutableContainers.Prelude
import MutableContainers.RefMonad
import qualified MutableContainers.WordArray as WordArray
import qualified MutableContainers.Array as Array


data Node m k v =
  Empty |
  Leaf !Hash !k !v |
  BitmapIndexed !(WordArray.WordArray m (Node m k v)) |
  Full !(Array.Array m (k, v)) |
  Collision !Hash !(Array.Array m (k, v))

type Hash = Int

type HashTable m k v = Ref m (Node m k v)

type HashTableMonad m = (RefMonad m, WordArray.WordArrayMonad m, Array.ArrayMonad m)

type Key k = (Eq k, Hashable k)

insert :: (Key k, HashTableMonad m) => HashTable m k v -> k -> v -> m ()
insert ref k v = do
  readRef ref >>= \case
    Empty -> writeRef ref (Leaf (hash k) k v)

  where

lookup :: (Key k, HashTableMonad m) => HashTable m k v -> k -> m (Maybe v)
lookup ref k = do
  readRef ref >>= \case
    Empty -> return Nothing
    Leaf h' k' v' -> 
      if h' == hash k && k' == k
        then return (Just v')
        else return Nothing
      
