import Data.Text (Text)
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Common

data Enum0
  = A
  | B
  | C

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize],
        dataInterfaces = [Parcelable],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable]
      }
  )
  ''Enum0

newtype Newtype0 = Newtype0 Text

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize],
        dataInterfaces = [Parcelable],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable]
      }
  )
  ''Newtype0

data Data0 = Data0
  { data0Field0 :: Int,
    data0Field1 :: Maybe Int
  }

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize],
        dataInterfaces = [Parcelable],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable]
      }
  )
  ''Data0

data Record0 = Record0
  { record0Field0 :: Int,
    record0Field1 :: Int
  }

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, Serializable],
        dataInterfaces = [LinkEnumInterface "Enum1"],
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
        dataInterfaces = [LinkEnumInterface "Enum1"],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable]
      }
  )
  ''Record1

data Enum1
  = DataCons0 Record0
  | DataCons1 Record1

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [RawAnnotation "Serializable(with = Enum1Serializer::class)"],
        dataInterfaces = [Parcelable],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable]
      }
  )
  ''Enum1

data Enum2
  = DataCons2 {enum2Field0 :: Int, enum2Field1 :: Int}
  | DataCons3 {enum2Field2 :: Text, enum2Field3 :: Text}

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, Serializable],
        dataInterfaces = [Parcelable],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable]
      }
  )
  ''Enum2

data Enum3
  = D
  | E

mobileGenWith
  ( defaultOptions
      { dataProtocols = [],
        dataRawValue = Just Str
      }
  )
  ''Enum3

data Enum4
  = F
  | G

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, Serializable],
        dataInterfaces = [Parcelable],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable],
        dataRawValue = Just Str
      }
  )
  ''Enum4

data Enum5
  = H
  | I

mobileGen ''Enum5

newtype Newtype1 = Newtype1 {newtype1 :: Text}

mobileGen ''Newtype1

newtype Newtype2 = Newtype2 {newtype2 :: Enum5}

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, Serializable],
        dataInterfaces = [Parcelable],
        dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable]
      }
  )
  ''Newtype2

newtype Newtype3 = Newtype3 {newtype3 :: Either String Int}

mobileGen ''Newtype3

main :: IO ()
main = hspec $ do
  describe "kotlin golden" $ do
    it "kotlinEnum0" $
      defaultGolden "kotlinEnum0" (showKotlin @Enum0)
    it "kotlinNewtype0" $
      defaultGolden "kotlinNewtype0" (showKotlin @Newtype0)
    it "kotlinNewtype1" $
      defaultGolden "kotlinNewtype1" (showKotlin @Newtype1)
    it "kotlinNewtype2" $
      defaultGolden "kotlinNewtype2" (showKotlin @Newtype2)
    it "kotlinNewtype3" $
      defaultGolden "kotlinNewtype3" (showKotlin @Newtype3)
    it "kotlinData0" $
      defaultGolden "kotlinData0" (showKotlin @Data0)
    it "kotlinEnum1" $
      defaultGolden "kotlinEnum1" (showKotlin @Enum1)
    it "kotlinRecord0" $
      defaultGolden "kotlinRecord0" (showKotlin @Record0)
    it "kotlinRecord1" $
      defaultGolden "kotlinRecord1" (showKotlin @Record1)
    it "kotlinEnum2" $
      defaultGolden "kotlinEnum2" (showKotlin @Enum2)
    it "kotlinEnum3" $
      defaultGolden "kotlinEnum3" (showKotlin @Enum3)
    it "kotlinEnum4" $
      defaultGolden "kotlinEnum4" (showKotlin @Enum4)
    it "kotlinEnum5" $
      defaultGolden "kotlinEnum5" (showKotlin @Enum5)
  describe "swift golden" $ do
    it "swiftEnum0" $
      defaultGolden "swiftEnum0" (showSwift @Enum0)
    it "swiftNewtype0" $
      defaultGolden "swiftNewtype0" (showSwift @Newtype0)
    it "swiftNewtype1" $
      defaultGolden "swiftNewtype1" (showSwift @Newtype1)
    it "swiftNewtype2" $
      defaultGolden "swiftNewtype2" (showSwift @Newtype2)
    it "swiftNewtype3" $
      defaultGolden "swiftNewtype3" (showSwift @Newtype3)
    it "swiftData0" $
      defaultGolden "swiftData0" (showSwift @Data0)
    it "swiftEnum1" $
      defaultGolden "swiftEnum1" (showSwift @Enum1)
    it "swiftRecord0" $
      defaultGolden "swiftRecord0" (showSwift @Record0)
    it "swiftRecord1" $
      defaultGolden "swiftRecord1" (showSwift @Record1)
    it "swiftEnum2" $
      defaultGolden "swiftEnum2" (showSwift @Enum2)
    it "swiftEnum3" $
      defaultGolden "swiftEnum3" (showSwift @Enum3)
    it "swiftEnum4" $
      defaultGolden "swiftEnum4" (showSwift @Enum4)
    it "swiftEnum5" $
      defaultGolden "swiftEnum5" (showSwift @Enum5)
