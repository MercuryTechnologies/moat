module SumOfProductWithUpperCaseTagsSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude hiding (Enum)

data Record1 = Record1
  { field :: Int }

data Enum
  = DataCons0
  | DataCons1 Record1

mobileGen ''Record1

mobileGenWith
  ( defaultOptions
      { constructorLowerFirst = False
      , dataAnnotations = [Parcelize, Serializable, SerialName]
      , dataInterfaces = [Parcelable]
      , dataProtocols = [Hashable, Codable]
      , sumOfProductEncodingOptions =
          SumOfProductEncodingOptions
            { encodingStyle = TaggedObjectStyle
            , sumAnnotations = [RawAnnotation "JsonClassDiscriminator(\"tag\")"]
            , tagFieldName = "tag"
            , contentsFieldName = "contents"
            }
      }
  )
  ''Enum

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "SumOfProductWithUpperCaseTagsSpec"
    it "swift" $
      defaultGolden ("swiftEnum" <> moduleName) (showSwift @Enum)
    it "kotlin" $
      defaultGolden ("kotlinEnum" <> moduleName) (showKotlin @Enum)
