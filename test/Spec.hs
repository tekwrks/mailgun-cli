import Test.Hspec

import qualified FlagsSpec

main :: IO ()
main =
  hspec $ do
    describe "Flags" $ FlagsSpec.tests

