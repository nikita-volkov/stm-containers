module STMContainers.WordArray where

import STMContainers.Prelude hiding (lookup, toList, traverse_)
import Data.Primitive.Array
import qualified STMContainers.Prelude as Prelude
import qualified STMContainers.WordArray.Indices as Indices
import qualified Focus


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

{-# INLINE indices #-}
indices :: WordArray e -> Indices
indices (WordArray b _) = b

{-# INLINE maxSize #-}
maxSize :: Int
maxSize = Indices.maxSize

-- |
-- An array with a single element at the specified index.
{-# INLINE singleton #-}
singleton :: Index -> e -> WordArray e
singleton i e = 
  let b = Indices.insert i 0
      a = runST $ newArray 1 e >>= unsafeFreezeArray
      in WordArray b a

{-# INLINE pair #-}
pair :: Index -> e -> Index -> e -> WordArray e
pair i e i' e' =
  WordArray is a
  where 
    is = Indices.fromList [i, i']
    a = 
      runST $ if 
        | i < i' -> do
          a <- newArray 2 e
          writeArray a 1 e'
          unsafeFreezeArray a
        | i > i' -> do
          a <- newArray 2 e
          writeArray a 0 e'
          unsafeFreezeArray a
        | i == i' -> do
          a <- newArray 1 e'
          unsafeFreezeArray a

-- |
-- Unsafe.
-- Assumes that the list is sorted and contains no duplicate indexes.
{-# INLINE fromList #-}
fromList :: [(Index, e)] -> WordArray e
fromList l = 
  runST $ do
    indices <- newSTRef 0
    array <- newArray (length l) undefined
    forM_ (zip l [0..]) $ \((i, e), ai) -> do
      modifySTRef indices $ Indices.insert i
      writeArray array ai e
    WordArray <$> readSTRef indices <*> unsafeFreezeArray array
  
{-# INLINE toList #-}
toList :: WordArray e -> [(Index, e)]
toList (WordArray is a) = do
  i <- Indices.toList is
  e <- indexArrayM a (Indices.position i is)
  return (i, e)

-- |
-- Convert into a list representation.
{-# INLINE toMaybeList #-}
toMaybeList :: WordArray e -> [Maybe e]
toMaybeList w = do
  i <- [0 .. pred Indices.maxSize] 
  return $ lookup i w

{-# INLINE elements #-}
elements :: WordArray e -> [e]
elements (WordArray indices array) =
  map (\i -> indexArray array (Indices.position i indices)) .
  Indices.toList $
  indices

-- |
-- Set an element value at the index.
{-# INLINE set #-}
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
{-# INLINE unset #-}
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
{-# INLINE lookup #-}
lookup :: Index -> WordArray e -> Maybe e
lookup i (WordArray b a) =
  if Indices.elem i b
    then Just (indexArray a (Indices.position i b))
    else Nothing

-- |
-- Lookup strictly, using 'indexArrayM'.
{-# INLINE lookupM #-}
lookupM :: Monad m => Index -> WordArray e -> m (Maybe e)
lookupM i (WordArray b a) =
  if Indices.elem i b
    then liftM Just (indexArrayM a (Indices.position i b))
    else return Nothing

-- |
-- Check, whether there is an element at the index.
{-# INLINE isSet #-}
isSet :: Index -> WordArray e -> Bool
isSet i = Indices.elem i . indices

-- |
-- Get the amount of elements.
{-# INLINE size #-}
size :: WordArray e -> Int
size = Indices.size . indices

{-# INLINE null #-}
null :: WordArray e -> Bool
null = Indices.null . indices

{-# INLINE traverse_ #-}
traverse_ :: Applicative f => (a -> f b) -> WordArray a -> f ()
traverse_ f =
  inline Prelude.traverse_ f . elements

{-# INLINE foldM #-}
foldM :: Monad m => (a -> b -> m a) -> a -> WordArray b -> m a
foldM step acc =
  inline Prelude.foldM step acc . elements

{-# INLINE focusM #-}
focusM :: Monad m => Focus.StrategyM m a r -> Index -> WordArray a -> m (r, WordArray a)
focusM f i w = do
  let em = lookup i w
  (r, c) <- f em
  let w' = case c of
        Focus.Keep -> w
        Focus.Remove -> case em of
          Nothing -> w
          Just _ -> unset i w
        Focus.Replace e' -> set i e' w
  return (r, w')
