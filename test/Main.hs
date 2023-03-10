import qualified Suites.Bimap
import qualified Suites.Map
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

main =
  defaultMain . testGroup "" $
    [ testGroup "Bimap" Suites.Bimap.tests,
      testGroup "Map" Suites.Map.tests
    ]
