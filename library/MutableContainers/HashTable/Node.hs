module MutableContainers.HashTable.Node where

import MutableContainers.Prelude
import MutableContainers.RefMonad
import Data.Primitive.Array
import qualified MutableContainers.WordArray as WordArray
import qualified MutableContainers.Array as Array
import qualified MutableContainers.HashTable.Node.Leaf as Leaf


data HAMT m k v =
  Empty |
  BitmapIndexed !(WordArray.WordArray m (HAMT m k v)) |
  Full !(Array.Array m (k, v)) |
  Single {-# UNPACK #-} !Hash !k !v |
  Multiple {-# UNPACK #-} !Hash {-# UNPACK #-} !(Array (k, v))


type Hash = Int

type HAMTMonad m = (WordArray.WordArrayMonad m, Array.ArrayMonad m)

type Key k = (Eq k, Hashable k)

type Depth = Int

-- insert :: (Key k, HAMTMonad m) => HAMT m k v -> Depth -> k -> v -> m (HAMT m k v)
-- insert hamt k v = 
--   case hamt of
--     Empty -> return (Single (hash k) k v)

-- lookup :: (Key k, HAMTMonad m) => HAMT m k v -> k -> m (Maybe v)
-- lookup ref k = do
--   readRef ref >>= \case
--     Empty -> return Nothing
--     Leaf h' k' v' -> 
--       if h' == hash k && k' == k
--         then return (Just v')
--         else return Nothing
      
