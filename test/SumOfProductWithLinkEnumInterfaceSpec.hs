module SumOfProductWithLinkEnumInterfaceSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude hiding (Enum)

data Record0 = Record0
  { record0Field0 :: Int,
    record0Field1 :: Int
  }

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, Serializable],
        dataInterfaces = [LinkEnumInterface "Enum"],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable]
      }
  )
  ''Record0

data Record1 = Record1
  { record1Field0 :: Int,
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

data Enum
  = DataCons0 Record0
  | DataCons1 Record1

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, Serializable],
        dataInterfaces = [Parcelable],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable]
      }
  )
  ''Enum

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "SumOfProductWithLinkEnumInterfaceSpec"
    it "swift" $ do
      defaultGolden ("swiftRecord0" <> moduleName) (showSwift @Record0)
    it "swift" $
      defaultGolden ("swiftRecord1" <> moduleName) (showSwift @Record1)
    it "swift" $
      defaultGolden ("swiftEnum" <> moduleName) (showSwift @Enum)
    it "kotlin" $
      defaultGolden ("kotlin-tagged-object-Record0" <> moduleName) (showKotlinTaggedObject @Record0)
    it "kotlin" $
      defaultGolden ("kotlin-tagged-flat-object-Record0" <> moduleName) (showKotlinTaggedFlatObject @Record0)
    it "kotlin" $
      defaultGolden ("kotlin-tagged-object-Record1" <> moduleName) (showKotlinTaggedObject @Record1)
    it "kotlin" $
      defaultGolden ("kotlin-tagged-flat-object-Record1" <> moduleName) (showKotlinTaggedFlatObject @Record1)
    it "kotlin" $
      defaultGolden ("kotlin-tagged-object-Enum" <> moduleName) (showKotlinTaggedObject @Enum)
    it "kotlin" $
      defaultGolden ("kotlin-tagged-flat-object-Enum" <> moduleName) (showKotlinTaggedFlatObject @Enum)
