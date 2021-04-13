{-# language
    AllowAmbiguousTypes,
    DataKinds,
    TemplateHaskell,
    TypeApplications,
    ScopedTypeVariables
#-}

{-# options_ghc -Wno-unused-top-binds #-}

import Data.Text (Text)
import Data.Proxy (Proxy (..))
import Moat
import Test.Hspec
import           Test.Hspec.Golden

data Enum0
  = A
  | B
  | C
mobileGenWith
  (defaultOptions {dataAnnotations = [Parcelize], dataInterfaces = [Parcelable]})
  ''Enum0

newtype Newtype0 = Newtype0 Text
mobileGenWith
  (defaultOptions {dataAnnotations = [Parcelize], dataInterfaces = [Parcelable]})
  ''Newtype0

data Data0 =
  Data0 
    { data0Field0 :: Int,
      data0Field1 :: Maybe Int
    }
mobileGenWith
  (defaultOptions {dataAnnotations = [Parcelize], dataInterfaces = [Parcelable]})
  ''Data0

data Record0 =
  Record0
    { record0Field0 :: Int
    , record0Field1 :: Int
    }
mobileGenWith
  (defaultOptions {dataAnnotations = [Parcelize, Serializable], dataInterfaces = [LinkEnumInterface "Enum1"]})
  ''Record0

data Record1 =
  Record1
    { record1Field0 :: Int
    , record1Field1 :: Int
    }
mobileGenWith
  (defaultOptions {dataAnnotations = [Parcelize, Serializable], dataInterfaces = [LinkEnumInterface "Enum1"]})
  ''Record1

data Enum1
  = DataCons0 Record0
  | DataCons1 Record1
mobileGenWith
  (defaultOptions {dataAnnotations = [RawAnnotation "Serializable(with = Enum1Serializer::class)"], dataInterfaces = [Parcelable]})
  ''Enum1

data Enum2
  = DataCons2 { enum2Field0 :: Int, enum2Field1 :: Int }
  | DataCons3 { enum2Field2 :: Text, enum2Field3 :: Text }
mobileGenWith
  (defaultOptions {dataAnnotations = [Parcelize, Serializable], dataInterfaces = [Parcelable]})
  ''Enum2

showKotlin :: forall a. ToMoatData a => String
showKotlin = prettyKotlinData $ toMoatData (Proxy @a)

main :: IO ()
main = hspec $
  describe "stays golden" $ do
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
