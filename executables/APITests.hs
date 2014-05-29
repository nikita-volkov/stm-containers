{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import MutableContainers.Prelude

import {-@ HTF_TESTS @-} MutableContainers.WordArrayTests

main = htfMain $ htf_thisModulesTests : htf_importedTests
