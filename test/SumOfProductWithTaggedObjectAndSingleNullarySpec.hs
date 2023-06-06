module SumOfProductWithTaggedObjectAndSingleNullarySpec where

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
      }
  )
  ''Record0

data Enum
  = DataCons0 Record0
  | DataCons1

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, Serializable, SerialName]
      , dataInterfaces = [Parcelable]
      , sumOfProductEncodingOptions =
          SumOfProductEncodingOptions
            { encodingStyle = TaggedObjectStyle
            , sumAnnotations = [RawAnnotation "JsonClassDiscriminator(\"tag\")"]
            , contentsFieldName = "contents"
            , tagFieldName = "contents"
            }
      }
  )
  ''Enum

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "SumOfProductWithTaggedObjectAndSingleNullarySpec"
    it "kotlin" $
      defaultGolden ("kotlinRecord0" <> moduleName) (showKotlin @Record0)
    it "kotlin" $
      defaultGolden ("kotlinEnum" <> moduleName) (showKotlin @Enum)
