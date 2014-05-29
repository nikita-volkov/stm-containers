module MutableContainers.WordArray
(
  WordArray,
  Index,
  WordArrayMonad,
  singleton,
  set,
  unset,
  lookup,
  isSet,
  size,
  toList,
)
where

import MutableContainers.Prelude hiding (lookup, toList)
import Data.Primitive.Array
import Data.Primitive.MutVar
import Control.Monad.Primitive
import qualified MutableContainers.WordArray.Bitmap as Bitmap


-- | 
-- A bitmap of set elements.
type Bitmap = Bitmap.Bitmap

-- |
-- An index of an element.
type Index = Int

-- |
-- A monad, in which a 'WordArray' can be mutated.
class Monad m => WordArrayMonad m where
  -- |
  -- A space-efficient sparse array, 
  -- which can store only as many elements as there are bits in the machine word.
  data WordArray m e
  -- |
  -- An array with a single element at the specified index.
  singleton :: Index -> e -> m (WordArray m e)
  -- |
  -- Set an element value at the index.
  set :: WordArray m e -> Index -> e -> m ()
  -- |
  -- Remove an element.
  unset :: WordArray m e -> Index -> m ()
  -- |
  -- Lookup an item at the index.
  lookup :: WordArray m e -> Index -> m (Maybe e)
  getBitmap :: WordArray m e -> m Bitmap

-- |
-- Check, whether there is an element at the index.
isSet :: WordArrayMonad m => WordArray m e -> Index -> m Bool
isSet w i = getBitmap w >>= return . Bitmap.isSet i

-- |
-- Get the amount of elements.
size :: WordArrayMonad m => WordArray m e -> m Int
size w = getBitmap w >>= return . Bitmap.size

-- |
-- Convert into a list representation.
toList :: WordArrayMonad m => WordArray m e -> m [Maybe e]
toList w = forM [0 .. pred Bitmap.maxSize] $ lookup w



-- * Instances
-------------------------

instance WordArrayMonad STM where
  newtype WordArray STM e = STMWordArray (TVar (Bitmap, Array e))
  singleton i e =
    let b = Bitmap.set i 0
        a = runST $ newArray 1 e >>= unsafeFreezeArray
        in fmap STMWordArray (newTVar (b, a))
  set (STMWordArray v) i e = do
    (b, a) <- readTVar v
    let sparseIndex = Bitmap.sparseIndex i b
        size = Bitmap.size b
    if Bitmap.isSet i b
      then do
        let a' = runST $ do
              ma' <- newArray size undefined
              forM_ [0 .. (size - 1)] $ \i -> indexArrayM a i >>= writeArray ma' i
              writeArray ma' sparseIndex e
              unsafeFreezeArray ma'
        writeTVar v (b, a')
      else do
        let a' = runST $ do
              ma' <- newArray (size + 1) undefined
              forM_ [0 .. (sparseIndex - 1)] $ \i -> indexArrayM a i >>= writeArray ma' i
              writeArray ma' sparseIndex e
              forM_ [sparseIndex .. (size - 1)] $ \i -> indexArrayM a i >>= writeArray ma' (i + 1)
              unsafeFreezeArray ma'
            b' = Bitmap.set i b
        writeTVar v (b', a')
  unset (STMWordArray v) i = do
    (b, a) <- readTVar v
    if Bitmap.isSet i b
      then do
        let b' = Bitmap.invert i b
            a' = runST $ do
              ma' <- newArray (pred size) undefined
              forM_ [0 .. pred sparseIndex] $ \i -> indexArrayM a i >>= writeArray ma' i
              forM_ [succ sparseIndex .. pred size] $ \i -> indexArrayM a i >>= writeArray ma' (pred i)
              unsafeFreezeArray ma'
            sparseIndex = Bitmap.sparseIndex i b
            size = Bitmap.size b
        writeTVar v (b', a')
      else return ()
  lookup (STMWordArray v) i = do
    (b, a) <- readTVar v
    if Bitmap.isSet i b
      then return (Just (indexArray a (Bitmap.sparseIndex i b)))
      else return Nothing
  getBitmap (STMWordArray v) = readTVar v >>= return . fst

instance WordArrayMonad (ST s) where
  newtype WordArray (ST s) e = STWordArray (PrimWordArray (ST s) e)
  singleton i e = fmap STWordArray (primSingleton i e)
  set (STWordArray p) = primSet p
  unset (STWordArray p) = primUnset p
  lookup (STWordArray p) = primLookup p
  getBitmap (STWordArray p) = primGetBitmap p

instance WordArrayMonad IO where
  newtype WordArray IO e = IOWordArray (PrimWordArray IO e)
  singleton i e = fmap IOWordArray (primSingleton i e)
  set (IOWordArray p) = primSet p
  unset (IOWordArray p) = primUnset p
  lookup (IOWordArray p) = primLookup p
  getBitmap (IOWordArray p) = primGetBitmap p


-- * Shared Primitive Monads Implementation
-------------------------

type PrimWordArray m e = MutVar (PrimState m) (Bitmap, MutableArray (PrimState m) e)

primSingleton :: PrimMonad m => Int -> e -> m (PrimWordArray m e)
primSingleton i e = do
  let b = Bitmap.set i 0
  a <- newArray 1 e
  newMutVar (b, a)

primGetBitmap :: PrimMonad m => PrimWordArray m e -> m Bitmap
primGetBitmap = liftM fst . readMutVar

primSet :: PrimMonad m => PrimWordArray m e -> Int -> e -> m ()
primSet w i e = do
  (b, a) <- readMutVar w
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
      writeMutVar w (b', a')

primUnset :: PrimMonad m => PrimWordArray m e -> Int -> m ()
primUnset w i = do
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

primLookup :: PrimMonad m => PrimWordArray m e -> Int -> m (Maybe e)
primLookup w i = do
  (b, a) <- readMutVar w
  if Bitmap.isSet i b
    then liftM Just $ readArray a (Bitmap.sparseIndex i b)
    else return Nothing
      
