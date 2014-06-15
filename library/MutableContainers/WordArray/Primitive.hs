module MutableContainers.WordArray.Primitive where

import MutableContainers.Prelude hiding (lookup, toList)
import Data.Primitive.Array
import Data.Primitive.MutVar
import Control.Monad.Primitive
import qualified MutableContainers.WordArray.Bitmap as Bitmap

-- |
-- A mutable word array in primitive monads.
type WordArray s e = MutVar s (Bitmap, MutableArray s e)

-- | 
-- A bitmap of set elements.
type Bitmap = Bitmap.Bitmap

-- |
-- An index of an element.
type Index = Int

-- |
-- A word array determined by monad.
type MonadWordArray m e = WordArray (PrimState m) e

{-# INLINABLE singleton #-}
singleton :: PrimMonad m => Index -> e -> m (MonadWordArray m e)
singleton i e = do
  let bitmap = Bitmap.set i 0
  array <- newArray 1 e
  newMutVar (bitmap, array)

{-# INLINABLE bitmap #-}
bitmap :: PrimMonad m => MonadWordArray m e -> m Bitmap
bitmap = liftM fst . readMutVar

{-# INLINABLE set #-}
set :: PrimMonad m => MonadWordArray m e -> Index -> e -> m ()
set v i e = do
  (b, a) <- readMutVar v
  let sparseIndex = Bitmap.sparseIndex i b
  if Bitmap.isSet i b
    then do
      writeArray a sparseIndex e
    else do
      let b' = Bitmap.set i b
          size = Bitmap.size b
      a' <- newArray (size + 1) undefined
      forM_ [0 .. (sparseIndex - 1)] $ \i -> readArray a i >>= writeArray a' i
      writeArray a' sparseIndex e
      forM_ [sparseIndex .. (size - 1)] $ \i -> readArray a i >>= writeArray a' (i + 1)
      writeMutVar v (b', a')

{-# INLINABLE unset #-}
unset :: PrimMonad m => MonadWordArray m e -> Index -> m ()
unset w i = do
  (b, a) <- readMutVar w
  if Bitmap.isSet i b
    then do
      let b' = Bitmap.invert i b
          sparseIndex = Bitmap.sparseIndex i b
          size = Bitmap.size b
      a' <- newArray (pred size) undefined
      forM_ [0 .. pred sparseIndex] $ \i -> readArray a i >>= writeArray a' i
      forM_ [succ sparseIndex .. pred size] $ \i -> readArray a i >>= writeArray a' (pred i)
      writeMutVar w (b', a')
    else return ()

{-# INLINABLE lookup #-}
lookup :: PrimMonad m => MonadWordArray m e -> Index -> m (Maybe e)
lookup w i = do
  (b, a) <- readMutVar w
  if Bitmap.isSet i b
    then liftM Just $ readArray a (Bitmap.sparseIndex i b)
    else return Nothing
      
