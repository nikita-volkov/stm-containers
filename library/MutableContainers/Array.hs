module MutableContainers.Array where

import MutableContainers.Prelude
import qualified Data.Primitive.Array as Primitive


class Monad m => ArrayMonad m where
  data Array m a
  new :: Int -> m (Array m a)
  read :: Array m a -> Int -> m a
  write :: Array m a -> Int -> a -> m ()
  indexM :: Primitive.Array a -> Int -> m a
  unsafeFreeze :: Array m a -> m (Primitive.Array a)
  unsafeThaw :: Primitive.Array a -> m (Array m a)

type Index = Int

instance ArrayMonad STM where
  newtype Array STM e = STMArray (TVar (Primitive.Array e))

instance ArrayMonad (ST s) where
  newtype Array (ST s) e = STArray (Primitive.MutableArray s e)

instance ArrayMonad IO where
  newtype Array IO e = IOArray (Primitive.MutableArray RealWorld e)

