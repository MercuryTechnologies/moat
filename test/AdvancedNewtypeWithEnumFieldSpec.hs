module AdvancedNewtypeWithEnumFieldSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude hiding (Enum)

data Enum
  = A
  | B

mobileGen ''Enum

newtype Newtype = Newtype {newtypeField :: Enum}

mobileGen
  ''Newtype

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "AdvancedNewtypeWithEnumFieldSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @Newtype)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @Newtype)
    it "typescript" $
      defaultGolden ("typescript" <> moduleName) (showTypescript @Newtype)
