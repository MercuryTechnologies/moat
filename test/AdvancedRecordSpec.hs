module AdvancedRecordSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden

data Data = Data
  { field0 :: Int,
    field1 :: Maybe Int
  }

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize],
        dataInterfaces = [Parcelable],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable]
      }
  )
  ''Data

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "AdvancedRecordSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @Data)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @Data)
