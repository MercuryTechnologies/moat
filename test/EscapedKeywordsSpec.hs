module EscapedKeywordsSpec where

import Common
import Moat
import Test.Hspec
import Test.Hspec.Golden

data Struct = Struct
  { func :: Int
  , static :: String
  , public :: Bool
  }

mobileGenWith
  (defaultOptions {dataProtocols = [Codable]})
  ''Struct

data Keyword
  = Case
  | Default
  | Class
  | Switch
  | Return

mobileGen
  ''Keyword

data Expression
  = Try {defer :: Int, fallthrough :: String}
  | Catch

mobileGenWith
  (defaultOptions {dataProtocols = [Codable]})
  ''Expression

spec :: Spec
spec =
  describe "stays golden" $ do
    let moduleName = "EscapedKeywordsSpec"
    it "swift struct" $
      defaultGolden ("swiftStruct" <> moduleName) (showSwift @Struct)
    it "swift plain enum" $
      defaultGolden ("swiftKeyword" <> moduleName) (showSwift @Keyword)
    it "swift sum of products" $
      defaultGolden ("swiftExpression" <> moduleName) (showSwift @Expression)
