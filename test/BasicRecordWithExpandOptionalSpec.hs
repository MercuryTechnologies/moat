module BasicRecordWithExpandOptionalSpec where

import Common
import Moat
import Moat.Types (Options (..))
import Test.Hspec
import Test.Hspec.Golden

data Data = Data
  { field0 :: Int,
    field1 :: Maybe Int
  }

mobileGenWith
  defaultOptions {optionalExpand = True}
  ''Data

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "BasicRecordWithExpandOptionalSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @Data)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @Data)
