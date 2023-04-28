module StrictFieldsSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude hiding (Enum)

data Record = Record 
  { fieldA :: String
  , fieldB :: String
  }

-- | Note: you can't discard a strict field
mobileGenWith
  ( defaultOptions
      { strictFields = ["fieldA"]
      , omitFields = const Discard
      }
  )
  ''Record

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "StrictFieldsCheck"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @Record)
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @Record)

