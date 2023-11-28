{-# OPTIONS_GHC -Wno-orphans #-}

module GenericStructSpec where

import Common
import Data.Tree (Tree)
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude

mobileGenWith
  ( defaultOptions
      { dataInterfaces = [Parcelable]
      , dataProtocols = [Codable]
      , generateDocComments = False
      }
  )
  ''Tree

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "GenericStructSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @(Tree _))
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @(Tree _))
