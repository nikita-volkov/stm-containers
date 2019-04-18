{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Prelude

import {-@ HTF_TESTS @-} Main.MapTests
import {-@ HTF_TESTS @-} Main.BimapTests


main = htfMain $ htf_thisModulesTests : htf_importedTests
