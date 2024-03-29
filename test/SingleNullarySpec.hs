module SingleNullarySpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude hiding (Enum)

data Data = DataCons

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, Serializable, SerialName]
      , dataInterfaces = [Parcelable]
      , dataProtocols = [Hashable, Codable]
      }
  )
  ''Data

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "SingleNullarySpec"
    it "kotlin" $
      defaultGolden ("kotlinData" <> moduleName) (showKotlin @Data)
    it "swift" $
      defaultGolden ("swiftData" <> moduleName) (showSwift @Data)
