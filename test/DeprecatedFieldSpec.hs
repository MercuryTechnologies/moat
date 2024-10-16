module DeprecatedFieldSpec where

import Common
import Data.List (stripPrefix)
import Data.Maybe
import Moat
import Test.Hspec
import Test.Hspec.Golden

data Data = Data
  { testField0 :: Int
  , testField1 :: Maybe Int
  }

mobileGenWith
  ( defaultOptions
      { fieldsRequiredByClients = ["testField0", "testField1"]
      , omitFields = const Discard
      , deprecatedFields = [("testField1", Just "Deprecated since build 500")]
      , fieldLabelModifier = \s -> fromMaybe s (stripPrefix "test" s)
      }
  )
  ''Data

spec :: Spec
spec =
  fdescribe "stays golden" $ do
    let moduleName = "DeprecatedFieldSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @Data)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @Data)
