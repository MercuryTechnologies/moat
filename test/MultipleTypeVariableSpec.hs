module MultipleTypeVariableSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden

data Data a b = Data { field0 :: a, field1 :: b }

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize],
        dataInterfaces = [Parcelable],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable]
      }
  )
  ''Data

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "MultipleTypeVariableSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @(Data _ _))
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @(Data _ _))
