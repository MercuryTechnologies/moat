module StrictEnumsSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude hiding (Enum)
import Data.List (stripPrefix)

data EnumA
  = EnumFirst
  | EnumSecond
  | EnumThird

mobileGenWith
  ( defaultOptions
      { strictCases = ["EnumFirst"]
      , omitCases = const Discard
      }
  )
  ''EnumA

data EnumB
  = EnumFourth
  | EnumFifth
  | EnumSixth

mobileGenWith
  ( defaultOptions
      { strictCases = ["EnumFourth"]
      , omitCases = const Discard
      , constructorModifier = \case
        xs | Just rest <- stripPrefix "Enum" xs -> rest
        xs -> xs
      }
  )
  ''EnumB

spec :: Spec
spec = do
  describe "EnumA stays golden" $ do
    let moduleName = "StrictEnumsSpec-EnumA"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @EnumA)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @EnumA)
  describe "EnumB stays golden" $ do
    let moduleName = "StrictEnumsSpec-EnumB"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @EnumB)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @EnumB)
