module STMContainers.SizedArray where

import STMContainers.Prelude hiding (lookup, toList, foldM)
import Data.Primitive.Array
import qualified STMContainers.Prelude as Prelude
import qualified Focus

-- |
-- An array, 
-- which sacrifices the performance for space-efficiency and thread-safety.
data SizedArray a =
  SizedArray {-# UNPACK #-} !Int {-# UNPACK #-} !(Array a)

-- |
-- An index of an element.
type Index = Int

pair :: a -> a -> SizedArray a
pair e e' =
  runST $ do
    a <- newArray 2 e
    writeArray a 1 e'
    SizedArray 2 <$> unsafeFreezeArray a

-- |
-- Get the amount of elements.
size :: SizedArray a -> Int
size (SizedArray b _) = b

-- |
-- Get the amount of elements.
null :: SizedArray a -> Bool
null = (== 0) . size

find :: (a -> Bool) -> SizedArray a -> Maybe (Index, a)
find p (SizedArray s a) = loop 0
  where
    loop i = if i < s
      then let e = indexArray a i in if p e
        then Just (i, e)
        else loop (succ i)
      else Nothing

-- |
-- Unsafe. Doesn't check the index overflow.
insert :: Index -> a -> SizedArray a -> SizedArray a
insert i e (SizedArray s a) = 
  runST $ do
    m' <- newArray s undefined
    forM_ [0 .. pred s] $ \i' -> indexArrayM a i' >>= writeArray m' i'
    writeArray m' i e
    SizedArray s <$> unsafeFreezeArray m'

delete :: Index -> SizedArray a -> SizedArray a
delete i (SizedArray s a) = 
  runST $ do
    m' <- newArray (pred s) undefined
    forM_ [0 .. pred i] $ \i' -> indexArrayM a i' >>= writeArray m' i'
    forM_ [succ i .. pred s] $ \i' -> indexArrayM a i' >>= writeArray m' (pred i')
    SizedArray (pred s) <$> unsafeFreezeArray m'

append :: a -> SizedArray a -> SizedArray a
append e (SizedArray s a) =
  runST $ do
    m' <- newArray (succ s) undefined
    forM_ [0 .. pred s] $ \i -> indexArrayM a i >>= writeArray m' i
    writeArray m' s e
    SizedArray (succ s) <$> unsafeFreezeArray m'

foldM :: (Monad m) => (a -> b -> m a) -> a -> SizedArray b -> m a
foldM step acc (SizedArray size array) =
  Prelude.foldM step' acc [0 .. pred size]
  where
    step' acc' i = indexArrayM array i >>= step acc'
