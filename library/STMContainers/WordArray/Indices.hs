module STMContainers.WordArray.Indices where

import STMContainers.Prelude hiding (toList, traverse_)
import qualified STMContainers.Prelude as Prelude


-- |
-- A compact set of indices.
type Indices = Int

type Index = Int

type Position = Int

-- |
-- A number of indexes, preceding this one.
{-# INLINE position #-}
position :: Index -> Indices -> Position
position i b = popCount (b .&. (bit i - 1))

{-# INLINE singleton #-}
singleton :: Index -> Indices
singleton = bit

{-# INLINE insert #-}
insert :: Index -> Indices -> Indices
insert i = (bit i .|.)

{-# INLINE invert #-}
invert :: Index -> Indices -> Indices
invert i = (bit i `xor`)

{-# INLINE elem #-}
elem :: Index -> Indices -> Bool
elem = flip testBit

{-# INLINE size #-}
size :: Indices -> Int
size = popCount

{-# INLINE null #-}
null :: Indices -> Bool
null = (== 0)

{-# INLINE maxSize #-}
maxSize :: Int
maxSize = bitSize (undefined :: Indices)

{-# INLINE fromList #-}
fromList :: [Index] -> Indices
fromList = Prelude.foldr (.|.) 0 . map bit

{-# INLINE toList #-}
toList :: Indices -> [Index]
toList w = filter (testBit w) allIndices

{-# INLINE positions #-}
positions :: Indices -> [Position]
positions = enumFromTo 0 . pred . size

{-# NOINLINE allIndices #-}
allIndices :: [Index]
allIndices = [0 .. pred maxSize]

{-# INLINE foldr #-}
foldr :: (Index -> r -> r) -> r -> Indices -> r
foldr s r ix = 
  Prelude.foldr (\i r' -> if testBit ix i then s i r' else r') r allIndices
