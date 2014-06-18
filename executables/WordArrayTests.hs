{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Test.QuickCheck.Monadic
import STMContainers.Prelude
import STMContainers.Transformers
import Control.Monad.Trans.Free
import Control.Monad.Free.TH
import qualified STMContainers.WordArray as WordArray
import qualified Data.Char as Char
import qualified WordArrayTests.Update as Update


main = htfMain $ htf_thisModulesTests

prop_differentInterpretersProduceSameResults (update :: Update.Update Char ()) =
  Update.interpretMaybeList update ==
  fmap WordArray.toList (Update.interpretWordArray update)

