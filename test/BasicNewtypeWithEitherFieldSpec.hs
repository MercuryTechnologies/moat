module BasicNewtypeWithEitherFieldSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden

newtype Newtype = Newtype {newtypeField :: Either String Int}

mobileGen
  ''Newtype

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "BasicNewtypeWithEitherFieldSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @Newtype)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @Newtype)
