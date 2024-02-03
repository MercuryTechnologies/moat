module SumOfProductWithTaggedFlatObjectStyleSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude hiding (Enum)

data Record0 = Record0
  { record0Field0 :: Int
  , record0Field1 :: Int
  }

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, Serializable]
      , dataInterfaces = [Parcelable]
      , dataProtocols = [Codable]
      }
  )
  ''Record0

data Record1 = Record1
  { record1Field0 :: Int
  , record1Field1 :: Int
  }

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, Serializable]
      , dataInterfaces = [Parcelable]
      , dataProtocols = [Codable]
      }
  )
  ''Record1

data Enum
  = DataCons0 Record0
  | DataCons1 Record1
  | DataCons2

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, Serializable, SerialName]
      , dataInterfaces = [Parcelable]
      , dataProtocols = [Codable]
      , sumOfProductEncodingOptions =
          SumOfProductEncodingOptions
            { encodingStyle = TaggedFlatObjectStyle
            , sumAnnotations = [RawAnnotation "JsonClassDiscriminator(\"tag\")"]
            , tagFieldName = "tag"
            , contentsFieldName = "contents"
            }
      , enumUnknownCase = Just "_unknown"
      }
  )
  ''Enum

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "SumOfProductWithTaggedFlatObjectStyleSpec"
    it "kotlin" $
      defaultGolden ("kotlinRecord0" <> moduleName) (showKotlin @Record0)
    it "kotlin" $
      defaultGolden ("kotlinRecord1" <> moduleName) (showKotlin @Record1)
    it "kotlin" $
      defaultGolden ("kotlinEnum" <> moduleName) (showKotlin @Enum)
    it "swift" $
      defaultGolden ("swiftRecord0" <> moduleName) (showSwift @Record0)
    it "swift" $
      defaultGolden ("swiftRecord1" <> moduleName) (showSwift @Record1)
    it "swift" $
      defaultGolden ("swiftEnum" <> moduleName) (showSwift @Enum)
