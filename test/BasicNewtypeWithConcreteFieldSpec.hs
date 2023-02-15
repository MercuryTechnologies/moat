module BasicNewtypeWithConcreteFieldSpec where

import Common
import Data.Text (Text)
import Moat
import Test.Hspec
import Test.Hspec.Golden

newtype Newtype = Newtype {newtypeField :: Text}

mobileGen
  ''Newtype

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "BasicNewtypeWithConcreteFieldSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @Newtype)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @Newtype)
    it "typescript" $
      defaultGolden ("typescript" <> moduleName) (showTypescript @Newtype)
