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
import qualified MutableContainers.WordArray.Primitive as Primitive


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
  newtype WordArray (ST s) e = STWordArray (Primitive.WordArray s e)
  singleton i e = fmap STWordArray (Primitive.singleton i e)
  set (STWordArray p) = Primitive.set p
  unset (STWordArray p) = Primitive.unset p
  lookup (STWordArray p) = Primitive.lookup p
  getBitmap (STWordArray p) = Primitive.bitmap p

instance WordArrayMonad IO where
  newtype WordArray IO e = IOWordArray (Primitive.WordArray RealWorld e)
  singleton i e = fmap IOWordArray (Primitive.singleton i e)
  set (IOWordArray p) = Primitive.set p
  unset (IOWordArray p) = Primitive.unset p
  lookup (IOWordArray p) = Primitive.lookup p
  getBitmap (IOWordArray p) = Primitive.bitmap p


