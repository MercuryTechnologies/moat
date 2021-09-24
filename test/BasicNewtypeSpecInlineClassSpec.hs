module BasicNewtypeSpecInlineClassSpec where

import Common
import Data.Text (Text)
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude

newtype Newtype = Newtype Text

mobileGenWith
  defaultOptions { newtypeDeclaration = InlineClass }
  ''Newtype

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "BasicNewtypeSpecInlineClassSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @Newtype)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @Newtype)
