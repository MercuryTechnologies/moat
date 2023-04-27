module BasicNewtypeJoinOptionalsSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude

newtype Newtype = Newtype (Maybe (Maybe Int))

mobileGen
  ''Newtype

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "BasicNewtypeJoinOptionalsSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @Newtype)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @Newtype)
