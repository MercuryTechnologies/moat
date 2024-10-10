module DeprecatedFieldSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden

data Data = Data
  { field0 :: Int
  , field1 :: Maybe Int
  }

mobileGenWith
  ( defaultOptions
      { fieldsRequiredByClients = ["field0", "field1"]
      , omitFields = const Discard
      , deprecatedFields = [("field1", Just "Deprecated since build 500")]
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
