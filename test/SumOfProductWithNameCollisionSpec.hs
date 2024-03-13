module SumOfProductWithNameCollisionSpec where

import Common
import Data.List (stripPrefix)
import Data.Maybe
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
  = EnumRecord0 Record0
  | EnumRecord1 Record1
  | EnumRecord2

mobileGenWith
  ( defaultOptions
      { constructorModifier = \body -> fromMaybe body (stripPrefix "Enum" body)
      , dataAnnotations = [Parcelize, Serializable, SerialName]
      , dataInterfaces = [Parcelable]
      , dataProtocols = [Codable]
      , sumOfProductEncodingOptions =
          SumOfProductEncodingOptions
            { encodingStyle = TaggedObjectStyle
            , sumAnnotations = [RawAnnotation "JsonClassDiscriminator(\"tag\")"]
            , contentsFieldName = "contents"
            , tagFieldName = "tag"
            }
      }
  )
  ''Enum

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "SumOfProductWithNameCollisionSpec"
    it "kotlin" $
      defaultGolden ("kotlinEnum" <> moduleName) (showKotlin @Enum)
    it "swift" $
      defaultGolden ("swiftEnum" <> moduleName) (showSwift @Enum)
