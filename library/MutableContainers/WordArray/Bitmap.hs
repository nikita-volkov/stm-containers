module MutableContainers.WordArray.Bitmap where

import MutableContainers.Prelude


type Bitmap = Word


sparseIndex :: Int -> Bitmap -> Int
sparseIndex i b = popCount (b .&. (bit i - 1))

singleton :: Int -> Bitmap
singleton = bit

set :: Int -> Bitmap -> Bitmap
set i = (bit i .|.)

invert :: Int -> Bitmap -> Bitmap
invert i = (bit i `xor`)

isSet :: Int -> Bitmap -> Bool
isSet = flip testBit

size :: Bitmap -> Int
size = popCount

maxSize :: Int
maxSize = bitSize (undefined :: Bitmap)
