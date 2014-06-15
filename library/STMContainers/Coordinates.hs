module STMContainers.Coordinates where

import STMContainers.Prelude


type Coordinates = [Int]

type ChunkSize = Int

type ChunkMask = Int

type Depth = Int

type Index = Int

-- | 
-- Convert a global index into tree-coordinates using chunk-size and depth.
-- 
coordinates :: ChunkSize -> ChunkMask -> Depth -> Index -> Coordinates
coordinates chunkSize chunkMask depth i =
  if depth > 0
    then
      chunkMask .&. i :
      coordinates chunkSize chunkMask (pred depth) (unsafeShiftR i chunkSize)
    else
      []


