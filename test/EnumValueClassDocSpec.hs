module EnumValueClassDocSpec where

import Common
import Control.Monad (when)
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude hiding (Enum)

-- | Top-level enum documentation.
data EnumAsValueClass
  = -- | 'First'
    First
  | -- | 'Second'
    Second
  | -- | 'Third'
    Third

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, Serializable]
      , dataInterfaces = [Parcelable]
      , dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable]
      , enumEncodingStyle = ValueClassStyle
      }
  )
  ''EnumAsValueClass

spec :: Spec
spec =
  when hasDoc $ do
    describe "stays golden" $ do
      let moduleName = "EnumValueClassDocSpec"
      it "swift" $
        defaultGolden ("swift" <> moduleName) (showSwift @EnumAsValueClass)
      it "kotlin" $
        defaultGolden ("kotlin" <> moduleName) (showKotlin @EnumAsValueClass)
