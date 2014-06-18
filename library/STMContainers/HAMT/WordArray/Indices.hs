module STMContainers.HAMT.WordArray.Indices where

import STMContainers.Prelude hiding (toList, traverse_)
import qualified STMContainers.Prelude as Prelude
import qualified BitArray


-- |
-- A compact set of indices.
type Indices = BitArray.BitArray Int

type Index = Int

-- |
-- A number of indexes, preceding this one.
position :: Index -> Indices -> Int
position i b = popCount (b .&. (bit i - 1))

singleton :: Index -> Indices
singleton = bit

insert :: Index -> Indices -> Indices
insert i = (bit i .|.)

invert :: Index -> Indices -> Indices
invert i = (bit i `xor`)

elem :: Index -> Indices -> Bool
elem = flip testBit

size :: Indices -> Int
size = popCount

maxSize :: Int
maxSize = bitSize (undefined :: Indices)

toList :: Indices -> [Index]
toList w = filter (testBit w) allIndices

{-# NOINLINE allIndices #-}
allIndices :: [Index]
allIndices = [0 .. pred maxSize]

traverse_ :: Applicative f => (Index -> f b) -> Indices -> f ()
traverse_ f = inline Prelude.traverse_ f . inline toList
