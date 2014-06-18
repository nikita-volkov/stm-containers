module STMContainers.HAMT.WordArray where

import STMContainers.Prelude hiding (lookup, toList, traverse_)
import Data.Primitive.Array
import qualified STMContainers.Prelude as Prelude
import qualified STMContainers.HAMT.WordArray.Indices as Indices


-- |
-- An immutable space-efficient sparse array, 
-- which can store only as many elements as there are bits in the machine word.
data WordArray e =
  WordArray {-# UNPACK #-} !Indices {-# UNPACK #-} !(Array e)

-- | 
-- A bitmap of set elements.
type Indices = Indices.Indices

-- |
-- An index of an element.
type Index = Int

-- |
-- An array with a single element at the specified index.
singleton :: Index -> e -> WordArray e
singleton i e = 
  let b = Indices.insert i 0
      a = runST $ newArray 1 e >>= unsafeFreezeArray
      in WordArray b a

fromList :: [(Int, e)] -> WordArray e
fromList = $notImplemented

-- |
-- Set an element value at the index.
set :: Index -> e -> WordArray e -> WordArray e
set i e (WordArray b a) = 
  let 
    sparseIndex = Indices.position i b
    size = Indices.size b
    in if Indices.elem i b
      then 
        let a' = runST $ do
              ma' <- newArray size undefined
              forM_ [0 .. (size - 1)] $ \i -> indexArrayM a i >>= writeArray ma' i
              writeArray ma' sparseIndex e
              unsafeFreezeArray ma'
            in WordArray b a'
      else
        let a' = runST $ do
              ma' <- newArray (size + 1) undefined
              forM_ [0 .. (sparseIndex - 1)] $ \i -> indexArrayM a i >>= writeArray ma' i
              writeArray ma' sparseIndex e
              forM_ [sparseIndex .. (size - 1)] $ \i -> indexArrayM a i >>= writeArray ma' (i + 1)
              unsafeFreezeArray ma'
            b' = Indices.insert i b
            in WordArray b' a'

-- |
-- Remove an element.
unset :: Index -> WordArray e -> WordArray e
unset i (WordArray b a) =
  if Indices.elem i b
    then
      let 
        b' = Indices.invert i b
        a' = runST $ do
          ma' <- newArray (pred size) undefined
          forM_ [0 .. pred sparseIndex] $ \i -> indexArrayM a i >>= writeArray ma' i
          forM_ [succ sparseIndex .. pred size] $ \i -> indexArrayM a i >>= writeArray ma' (pred i)
          unsafeFreezeArray ma'
        sparseIndex = Indices.position i b
        size = Indices.size b
        in WordArray b' a'
    else WordArray b a

-- |
-- Lookup an item at the index.
lookup :: Index -> WordArray e -> Maybe e
lookup i (WordArray b a) =
  if Indices.elem i b
    then Just (indexArray a (Indices.position i b))
    else Nothing

indices :: WordArray e -> Indices
indices (WordArray b _) = b

-- |
-- Check, whether there is an element at the index.
isSet :: Index -> WordArray e -> Bool
isSet i = Indices.elem i . indices

-- |
-- Get the amount of elements.
size :: WordArray e -> Int
size = Indices.size . indices

-- |
-- Convert into a list representation.
toList :: WordArray e -> [Maybe e]
toList w = do
  i <- [0 .. pred Indices.maxSize] 
  return $ lookup i w

elements :: WordArray e -> [e]
elements (WordArray indices array) =
  map (\i -> indexArray array (Indices.position i indices)) .
  inline Indices.toList $
  indices

traverse_ :: Applicative f => (a -> f b) -> WordArray a -> f ()
traverse_ f =
  inline Prelude.traverse_ f . inline elements

foldM :: Monad m => (a -> b -> m a) -> a -> WordArray b -> m a
foldM step acc =
  inline Prelude.foldM step acc . inline elements
