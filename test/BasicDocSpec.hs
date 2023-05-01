{-# LANGUAGE CPP #-}

module BasicDocSpec where

import Common
import Control.Monad (when)
import Moat
import Test.Hspec
import Test.Hspec.Golden

-- | = Heading
-- == /Second-level heading, italic/
--
-- Paragraphs of documentation for the 'Data' type. This line
-- should wrap according to your preference.
--
-- Some additional markup: /emphasis/, __bold__, @monospaced@.
--
-- === __Lists__
--
-- Unordered
--
-- * Unordered item
-- * Second unordered item
-- * Third unordered item
--
-- Ordered
--
-- 1. Ordered item
-- 2. Second ordrered item
-- 3. Third ordered item
--
-- == Code block
--
-- > func fibonacci(_ n: Int) -> Int {
-- >     guard n != 0, n != 1 else { return n }
-- >     return fibonacci(n - 1) + fibonacci(n - 2)
-- > }
data Data = Data
  { first :: Int
  -- ^ First field, it's an Int
  , second :: Maybe Int
  -- ^ Second field, it's maybe an Int
  }

mobileGen
  ''Data

spec :: Spec
spec =
  when hasDoc $ do
    describe "stays golden" $ do
      let moduleName = "BasicDocSpec"
      it "swift" $
        defaultGolden ("swift" <> moduleName) (showSwift @Data)
      it "kotlin" $
        defaultGolden ("kotlin" <> moduleName) (showKotlin @Data)
