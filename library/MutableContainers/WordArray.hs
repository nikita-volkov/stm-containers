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
import qualified MutableContainers.WordArray.Immutable as Immutable


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
  newtype WordArray STM e = STMWordArray (TVar (Immutable.WordArray e))
  singleton i e = fmap STMWordArray (newTVar (Immutable.singleton i e))
  set (STMWordArray v) i e = readTVar v >>= writeTVar v . Immutable.set i e
  unset (STMWordArray v) i = readTVar v >>= writeTVar v . Immutable.unset i
  lookup (STMWordArray v) i = readTVar v >>= return . Immutable.lookup i
  getBitmap (STMWordArray v) = readTVar v >>= return . Immutable.bitmap

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


