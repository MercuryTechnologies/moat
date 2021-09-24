module BasicEnumSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude hiding (Enum)

data Enum
  = A
  | B
  | C

mobileGen
  ''Enum

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "BasicEnumSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @Enum)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @Enum)
    it "typescript" $
      defaultGolden ("typescript" <> moduleName) (showTypescript @Enum)
