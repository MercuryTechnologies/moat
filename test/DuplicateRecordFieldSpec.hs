{- HLINT ignore "Avoid restricted extensions" -}
{-# LANGUAGE DuplicateRecordFields #-}
module DuplicateRecordFieldSpec where

import Common
import Control.Monad (when)
import Moat
import Test.Hspec
import Test.Hspec.Golden

-- | Record 0 with duplicate fields
data Data0 = Data0
  { field0 :: Int,      -- ^ duplicate field0
    field1 :: Maybe Int -- ^ not a duplicate
  }

-- | Record 1 with duplicate fields
data Data1 = Data1
  { field0 :: String,      -- ^ duplicate field0
    field2 :: Maybe String -- ^ not a duplicate
  }

mobileGen ''Data0
mobileGen ''Data1

spec :: Spec
spec =
  when hasDoc $ do
    describe "stays golden" $ do
      let moduleName = "DuplicateRecordFieldSpec"
      it "swift" $
        defaultGolden ("swiftRecord0" <> moduleName) (showSwift @Data0)
      it "swift" $
        defaultGolden ("swiftRecord1" <> moduleName) (showSwift @Data1)
      it "kotlin" $
        defaultGolden ("kotlinRecord0" <> moduleName) (showKotlin @Data0)
      it "kotlin" $
        defaultGolden ("kotlinRecord1" <> moduleName) (showKotlin @Data1)
