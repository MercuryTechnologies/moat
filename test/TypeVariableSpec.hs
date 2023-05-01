{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module TypeVariableSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden

{-# HLINT ignore "Use newtype instead of data" #-}
data Data a = Data {field0 :: a}

mobileGenWith
  ( defaultOptions
      { dataAnnotations = [Parcelize]
      , dataInterfaces = [Parcelable]
      , dataProtocols = [OtherProtocol "CaseIterable", Hashable, Codable]
      }
  )
  ''Data

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "TypeVariableSpec"
    it "swift" $
      defaultGolden ("swift" <> moduleName) (showSwift @(Data _))
    it "kotlin" $
      defaultGolden ("kotlin" <> moduleName) (showKotlin @(Data _))
