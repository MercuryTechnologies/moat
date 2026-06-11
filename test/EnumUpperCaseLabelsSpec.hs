module EnumUpperCaseLabelsSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude hiding (Enum)

data Enum
  = AliceInChains
  | BlindMelon
  | Candlebox

mobileGenWith
  ( defaultOptions
      { constructorLowerFirst = False
      , dataRawValue = Just Str
      }
  )
  ''Enum

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "EnumUpperCaseLabelsSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @Enum)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @Enum)
