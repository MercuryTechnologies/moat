module StrictFieldsSpec where

import Common
import Data.List (stripPrefix)
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude hiding (Enum)

data RecordA = RecordA
  { fieldA :: String
  , fieldB :: String
  }

-- | Note: you can't discard a strict field
mobileGenWith
  ( defaultOptions
      { fieldsRequiredByClients = ["fieldA"]
      , omitFields = const Discard
      }
  )
  ''RecordA

data RecordB = RecordB
  { fieldC :: String
  , fieldD :: String
  }

-- | Note: you can't discard a strict field
mobileGenWith
  ( defaultOptions
      { fieldLabelModifier = \case
          xs | Just rest <- stripPrefix "field" xs -> rest
          xs -> xs
      , fieldsRequiredByClients = ["fieldC"]
      , omitFields = const Discard
      }
  )
  ''RecordB

spec :: Spec
spec = do
  describe "RecordA stays golden" $ do
    let moduleName = "StrictFieldsCheck-RecordA"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @RecordA)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @RecordA)
  describe "RecordB stays golden" $ do
    let moduleName = "StrictFieldsCheck-RecordB"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @RecordB)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @RecordB)
