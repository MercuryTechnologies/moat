module AdvancedEnumSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude hiding (Enum)

data Enum
  = A
  | B
  | C

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize],
        dataInterfaces = [Parcelable],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable]
      }
  )
  ''Enum

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "AdvancedEnumSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @Enum)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @Enum)
    it "typescript" $
      defaultGolden ("typescript" <> moduleName) (showTypescript @Enum)
