#!/usr/bin/env cabal
{- cabal:
build-depends:
  , base
  , bytestring
  , commonmark ==0.3.1
  , directory
  , filepath
  , text
default-extensions: LambdaCase, OverloadedStrings, ScopedTypeVariables
-}

-- | Check that two trees of generated Swift and Kotlin files render the same
-- documentation.
--
-- Usage: cabal run scripts/DocParity.hs -- BASE_DIR NEW_DIR
--
-- Every line outside doc comments must be identical. Each doc comment, with
-- its comment syntax stripped, must parse under commonmark to the same HTML.
-- Exits nonzero and prints every difference otherwise.
module Main (main) where

import Commonmark (Html, commonmark, renderHtml)
import Control.Monad (filterM, forM, when)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

data Segment
  = Code T.Text
  | -- | Doc comment kind and indentation, and its Markdown lines.
    Doc T.Text [T.Text]

main :: IO ()
main =
  getArgs >>= \case
    [base, new] -> do
      baseFiles <- listFiles base
      newFiles <- listFiles new
      let missing = filter (`notElem` newFiles) baseFiles ++ filter (`notElem` baseFiles) newFiles
      results <- forM (filter (`elem` newFiles) baseFiles) $ \file -> do
        old <- readUtf8 (base </> file)
        cur <- readUtf8 (new </> file)
        pure (compareFile file (segments old) (segments cur))
      let docBlocks = foldr ((+) . fst) 0 results
          problems = map (("only in one tree: " <>) . T.pack) missing ++ concatMap snd results
      mapM_ T.putStrLn problems
      putStrLn $
        show (length baseFiles)
          <> " files, "
          <> show docBlocks
          <> " doc blocks compared, "
          <> show (length problems)
          <> " mismatches"
      when (not (null problems)) exitFailure
    _ -> do
      putStrLn "usage: DocParity BASE_DIR NEW_DIR"
      exitFailure

readUtf8 :: FilePath -> IO T.Text
readUtf8 path =
  BS.readFile path >>= \bytes -> case T.decodeUtf8' bytes of
    Left err -> fail (path <> ": " <> show err)
    Right t -> pure t

listFiles :: FilePath -> IO [FilePath]
listFiles root = go ""
  where
    go rel = do
      entries <- listDirectory (root </> rel)
      files <- filterM (doesFileExist . (root </>)) (map (rel </>) entries)
      dirs <- filterM (doesDirectoryExist . (root </>)) (map (rel </>) entries)
      nested <- mapM go dirs
      pure (files ++ concat nested)

-- | Number of doc blocks compared, and a description of each mismatch.
compareFile :: FilePath -> [Segment] -> [Segment] -> (Int, [T.Text])
compareFile file = go (1 :: Int)
  where
    go n (a : as) (b : bs) =
      let (count, problems) = go (n + 1) as bs
       in case (a, b) of
            (Code x, Code y)
              | x == y -> (count, problems)
            (Doc kx x, Doc ky y)
              | kx == ky && html x == html y -> (count + 1, problems)
              | kx == ky -> (count + 1, report n a b : problems)
            _ -> (count, report n a b : problems)
    go _ [] [] = (0, [])
    go n as bs = (0, [header n <> "different number of segments: " <> T.pack (show (length as, length bs))])
    header n = T.pack file <> " segment " <> T.pack (show n) <> ": "
    report n a b = header n <> "\n--- base\n" <> describe a <> "\n+++ new\n" <> describe b
    describe = \case
      Code x -> x
      Doc k x -> k <> "\n" <> T.unlines x <> "html: " <> html x

html :: [T.Text] -> T.Text
html ls = case commonmark "doc" (T.unlines ls) of
  Left err -> "parse error: " <> T.pack (show err)
  Right (h :: Html ()) -> TL.toStrict (renderHtml h)

-- | Split a file into code lines and doc comment blocks: runs of Swift @///@
-- lines, and Kotlin @/** ... */@ comments.
segments :: T.Text -> [Segment]
segments = go . T.lines
  where
    go = \case
      [] -> []
      (l : ls)
        | "///" `T.isPrefixOf` stripped l ->
            let (block, rest) = span (("///" `T.isPrefixOf`) . stripped) (l : ls)
             in Doc (indent l <> "///") (map (dropSpace . T.drop 3 . stripped) block) : go rest
        | "/**" `T.isPrefixOf` stripped l && "*/" `T.isSuffixOf` stripped l ->
            Doc (indent l <> "/**") [T.strip (T.dropEnd 2 (T.drop 3 (stripped l)))] : go ls
        | "/**" `T.isPrefixOf` stripped l ->
            let (block, rest) = break (("*/" `T.isPrefixOf`) . stripped) ls
             in Doc (indent l <> "/**") (map (dropSpace . T.drop 1 . stripped) block) : go (drop 1 rest)
        | otherwise -> Code l : go ls
    stripped = T.stripStart
    indent = T.takeWhile (== ' ')
    dropSpace t = fromMaybe t (T.stripPrefix " " t)
