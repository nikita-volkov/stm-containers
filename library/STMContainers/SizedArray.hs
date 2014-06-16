module STMContainers.SizedArray where

import STMContainers.Prelude hiding (lookup, toList)
import Data.Primitive.Array

-- |
-- An array, 
-- which sacrifices the performance for space-efficiency and thread-safety.
data SizedArray a =
  SizedArray {-# UNPACK #-} !Int {-# UNPACK #-} !(Array a)

-- |
-- An index of an element.
type Index = Int

fromList :: [a] -> SizedArray a
fromList = $notImplemented

-- |
-- Get the amount of elements.
size :: SizedArray a -> Int
size (SizedArray b _) = b

-- |
-- Convert into a list representation.
toList :: SizedArray a -> [a]
toList w = $notImplemented

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
set :: Int -> a -> SizedArray a -> SizedArray a
set i e (SizedArray s a) = SizedArray s a'
  where
    a' = 
      runST $ do
        ma' <- newArray s undefined
        forM_ [0 .. pred s] $ \i' -> indexArrayM a i' >>= writeArray ma' i'
        writeArray ma' i e
        unsafeFreezeArray ma'

-- |
-- Map and also check, whether anything changed.
map' :: (a -> Maybe a) -> SizedArray a -> Maybe (SizedArray a)
map' = $notImplemented

append :: a -> SizedArray a -> SizedArray a
append = $notImplemented

filter :: (a -> Bool) -> SizedArray a -> SizedArray a
filter = $notImplemented