import qualified HeartsJsonSpec
import qualified Json.DecodeSpec
import Test.Hspec

main :: IO ()
main = Test.Hspec.hspec spec

spec :: Spec
spec = do
  Json.DecodeSpec.spec
  HeartsJsonSpec.spec
