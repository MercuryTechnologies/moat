module SumOfProductDocSpec where

import Common
import Control.Monad (when)
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude hiding (Enum)

-- | Documentation for 'Record0'.
data Record0 = Record0
  { record0Field0 :: Int,
    -- ^ The zeroth field of record 0
    record0Field1 :: Int
    -- ^ The first field of record 0
  }

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, Serializable],
        dataInterfaces = [LinkEnumInterface "Enum"],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable]
      }
  )
  ''Record0

-- | Documentation for 'Record1'.
data Record1 = Record1
  { -- | The zeroth field of record 1
    record1Field0 :: Int,
    -- | The first field of record 1
    record1Field1 :: Int
  }

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, Serializable],
        dataInterfaces = [LinkEnumInterface "Enum"],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable]
      }
  )
  ''Record1

-- | Top-level documentation describing 'Enum'.
data Enum
  = -- | A constructor.
    DataCons0 Record0
  | -- | Another constructor.
    DataCons1 Record1

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, Serializable, SerialName],
        dataInterfaces = [Parcelable],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable],
        sumOfProductEncodingOptions =
          SumOfProductEncodingOptions
            { encodingStyle = TaggedObjectStyle,
              sumAnnotations = [RawAnnotation "JsonClassDiscriminator(\"tag\")"],
              contentsFieldName = "contents"
            }
      }
  )
  ''Enum

spec :: Spec
spec =
  when hasDoc $ do
    describe "stays golden" $ do
      let moduleName = "SumOfProductDocSpec"
      it "swift" $ do
        defaultGolden ("swiftRecord0" <> moduleName) (showSwift @Record0)
      it "swift" $
        defaultGolden ("swiftRecord1" <> moduleName) (showSwift @Record1)
      it "swift" $
        defaultGolden ("swiftEnum" <> moduleName) (showSwift @Enum)
      it "kotlin" $
        defaultGolden ("kotlinRecord0" <> moduleName) (showKotlin @Record0)
      it "kotlin" $
        defaultGolden ("kotlinRecord1" <> moduleName) (showKotlin @Record1)
      it "kotlin" $
        defaultGolden ("kotlinEnum" <> moduleName) (showKotlin @Enum)
