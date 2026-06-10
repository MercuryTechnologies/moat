{-# LANGUAGE CPP #-}

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
  { field0 :: Int
  -- ^ duplicate field0
  , field1 :: Maybe Int
  -- ^ not a duplicate
  }

-- | Record 1 with duplicate fields
data Data1 = Data1
  { field0 :: String
  -- ^ duplicate field0
  , field2 :: Maybe String
  -- ^ not a duplicate
  }

mobileGen ''Data0
mobileGen ''Data1

-- | GHC 9.10 (template-haskell 2.22) fixed 'getDoc' for fields whose names are
--   made ambiguous by DuplicateRecordFields (GHC #17551). Earlier versions drop
--   the documentation for the duplicated field, producing different golden
--   output, so we key the golden files on the template-haskell version.
goldenName :: String -> String
#if MIN_VERSION_template_haskell(2,22,0)
goldenName prefix = prefix <> "DuplicateRecordFieldSpec"
#else
goldenName prefix = prefix <> "DuplicateRecordFieldSpecNoFieldDoc"
#endif

spec :: Spec
spec =
  when hasDoc $ do
    describe "stays golden" $ do
      it "swift" $
        defaultGolden (goldenName "swiftRecord0") (showSwift @Data0)
      it "swift" $
        defaultGolden (goldenName "swiftRecord1") (showSwift @Data1)
      it "kotlin" $
        defaultGolden (goldenName "kotlinRecord0") (showKotlin @Data0)
      it "kotlin" $
        defaultGolden (goldenName "kotlinRecord1") (showKotlin @Data1)
