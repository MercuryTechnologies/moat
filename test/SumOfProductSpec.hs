module SumOfProductSpec where

import Common
import Data.Text (Text)
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude hiding (Enum)

data Enum
  = DataCons0 {enumField0 :: Int, enumField1 :: Int}
  | DataCons1 {enumField2 :: Text, enumField3 :: Text}

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, Serializable],
        dataInterfaces = [Parcelable],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable]
      }
  )
  ''Enum

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "SumOfProductSpec"
    it "swift" $
      defaultGolden ("swiftEnum" <> moduleName) (showSwift @Enum)
    it "kotlin" $
      defaultGolden ("kotlinEnum" <> moduleName) (showKotlin @Enum)
    it "typescript" $
      defaultGolden ("typescript" <> moduleName) (showTypescript @Enum)
