module AdvancedEnumWithRawValueSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude hiding (Enum)

data Enum
  = A
  | B

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, Serializable],
        dataInterfaces = [Parcelable],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable],
        dataRawValue = Just Str
      }
  )
  ''Enum

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "AdvancedEnumWithRawValueSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @Enum)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @Enum)
