module SumOfProductWithTypeParameterSpec where

import Common
import Data.List (stripPrefix)
import Data.Maybe
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude hiding (Enum)

data CursorInput a
  = CursorInputNextPage (Maybe a)
  | CursorInputPreviousPage a
  | CursorInputUnknown

mobileGenWith
  ( defaultOptions
      { constructorModifier = \body -> fromMaybe body (stripPrefix "CursorInput" body)
      , dataAnnotations = [Serializable, SerialName]
      , dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable]
      , sumOfProductEncodingOptions =
          SumOfProductEncodingOptions
            { encodingStyle = TaggedObjectStyle
            , sumAnnotations = [RawAnnotation "JsonClassDiscriminator(\"direction\")"]
            , contentsFieldName = "key"
            , tagFieldName = "direction"
            }
      }
  )
  ''CursorInput

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "SumOfProductWithTypeParameterSpec"
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @(CursorInput _))
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @(CursorInput _))
