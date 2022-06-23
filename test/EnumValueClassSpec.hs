module EnumValueClassSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude hiding (Enum)

data EnumAsValueClass
  = First
  | Second
  | Third

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, Serializable],
        dataInterfaces = [Parcelable],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable],
        enumEncodingStyle = ValueClassStyle
      }
  )
  ''EnumAsValueClass

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "EnumValueClassSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @EnumAsValueClass)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @EnumAsValueClass)
