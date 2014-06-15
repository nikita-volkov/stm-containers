{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import STMContainers.Prelude
import STMContainers.Coordinates
import Numeric
import Data.Char
import NumericQQ


main = htfMain $ htf_thisModulesTests


test_1 = do
  assertEqual expected real
  where
    expected = [0, [bin|1010|], [bin|0100|]]
    real = reverse $ coordinates chunkSize chunkMask depth index where
      chunkSize = 4
      chunkMask = [bin|1111|]
      depth = 3
      index = [bin|000010100100|]
    

