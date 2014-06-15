module MutableContainers.HashTable.Node.Leaf where

import MutableContainers.Prelude
import MutableContainers.RefMonad
import Data.Primitive.Array

data Leaf k v =
  Single !k !v |
  Multiple {-# UNPACK #-} !(Array (k, v))

-- {-# INLINE lookup #-}
-- lookup :: (LeafMonad m, Eq k) => Leaf m k v -> k -> m (Maybe v)
-- lookup = \case
--   Single k v -> \k' -> return (if k == k' then Just v else Nothing)
--   -- Multiple a -> \k' -> 



