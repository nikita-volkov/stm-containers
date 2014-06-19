{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import STMContainers.Prelude

import {-@ HTF_TESTS @-} APITests.MapTests


main = htfMain $ htf_thisModulesTests : htf_importedTests
