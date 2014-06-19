module STMContainers.HAMT.Node.Level where

import STMContainers.Prelude hiding (mask)


-- |
-- A depth level of a node.
-- Must be a multiple of the 'step' value.
type Level = Int

hashIndex :: Level -> (Int -> Int)
hashIndex l i = mask .&. unsafeShiftR i l

mask :: Int
mask = bit step - 1

step :: Int
step = 5

limit :: Int
limit = bitSize (undefined :: Int)

succ :: Level -> Level
succ = (+ step)
