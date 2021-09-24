module BasicNewtypeSpec where

import Common
import Data.Text (Text)
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude

newtype Newtype = Newtype Text

mobileGen
  ''Newtype

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "BasicNewtypeSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @Newtype)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @Newtype)
