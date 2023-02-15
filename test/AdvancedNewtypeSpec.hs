module AdvancedNewtypeSpec where

import Common
import Data.Text (Text)
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude

newtype Newtype = Newtype Text

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, JvmInline],
        dataInterfaces = [Parcelable],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable]
      }
  )
  ''Newtype

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "AdvancedNewtypeSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @Newtype)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @Newtype)
    it "typescript" $
      defaultGolden ("typescript" <> moduleName) (showTypescript @Newtype)
