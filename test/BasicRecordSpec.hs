module BasicRecordSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden

data Data = Data
  { field0 :: Int,
    field1 :: Maybe Int
  }

mobileGen
  ''Data

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "BasicRecordSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @Data)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @Data)
    it "typescript" $
      defaultGolden ("typescript" <> moduleName) (showTypescript @Data)
