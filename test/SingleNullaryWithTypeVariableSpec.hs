module SingleNullaryWithTypeVariableSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude hiding (Enum)

data Data a = DataCons

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
    let moduleName = "SingleNullaryWithTypeVariableSpec"
    it "kotlin" $
      defaultGolden ("kotlinData" <> moduleName) (showKotlin @(Data Int))
    it "swift" $
      defaultGolden ("swiftData" <> moduleName) (showSwift @(Data Int))
