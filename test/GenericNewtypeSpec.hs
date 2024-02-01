{-# LANGUAGE DerivingStrategies #-}

module GenericNewtypeSpec where

import Common
import Data.List.NonEmpty (NonEmpty)
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude

newtype LTree a = LTree (NonEmpty a)
  deriving stock (Show, Eq)

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize, Serializable]
      , dataInterfaces = [Parcelable]
      , dataProtocols = [Hashable, Codable]
      , dataRawValue = Just Str
      , generateDocComments = False
      }
  )
  ''LTree

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "GenericNewtypeSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @(LTree _))
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @(LTree _))
