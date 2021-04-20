{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Moat
import Test.Hspec
import Test.Hspec.Golden

data Enum0
  = A
  | B
  | C

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize],
        dataInterfaces = [Parcelable],
        dataProtocols = [Hashable, Codable]
      }
  )
  ''Enum0

newtype Newtype0 = Newtype0 Text

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize],
        dataInterfaces = [Parcelable],
        dataProtocols = [Hashable, Codable]
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
        dataProtocols = [Hashable, Codable]
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
        dataProtocols = [Hashable, Codable]
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
        dataProtocols = [Hashable, Codable]
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
        dataProtocols = [Hashable, Codable]
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
        dataProtocols = [Hashable, Codable]
      }
  )
  ''Enum2

showKotlin :: forall a. ToMoatData a => String
showKotlin = prettyKotlinData $ toMoatData (Proxy @a)

showSwift :: forall a. ToMoatData a => String
showSwift = prettySwiftData $ toMoatData (Proxy @a)

main :: IO ()
main = hspec $ do
  describe "kotlin golden" $ do
    it "kotlinEnum0" $
      defaultGolden "kotlinEnum0" (showKotlin @Enum0)
    it "kotlinNewtype0" $
      defaultGolden "kotlinNewtype0" (showKotlin @Newtype0)
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
  describe "swift golden" $ do
    it "swiftEnum0" $
      defaultGolden "swiftEnum0" (showSwift @Enum0)
    it "swiftNewtype0" $
      defaultGolden "swiftNewtype0" (showSwift @Newtype0)
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
