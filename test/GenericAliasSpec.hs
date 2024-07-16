{-# LANGUAGE DerivingStrategies #-}

module GenericAliasSpec where

import Common
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Moat
import Test.Hspec
import Test.Hspec.Golden
import Prelude

newtype TreeResponse a = TreeResponse (NonEmpty a)
  deriving stock (Show, Eq)

instance ToMoatType a => ToMoatType (TreeResponse a) where
  toMoatType _ = Concrete "TreeResponse" [toMoatType (Proxy @a)]

instance ToMoatData (TreeResponse a) where
  toMoatData _ =
    MoatAlias
      { aliasName = "TreeResponse"
      , aliasDoc = Nothing
      , aliasTyVars = ["a"]
      , aliasTyp = Concrete "Tree" [Poly "a"]
      }

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "GenericAliasSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @(TreeResponse _))
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @(TreeResponse _))
