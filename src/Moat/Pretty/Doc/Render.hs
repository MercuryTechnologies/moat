{-# LANGUAGE OverloadedStrings #-}

module Moat.Pretty.Doc.Render
  ( renderMarkdown,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Moat.Pretty.Doc.Markdown

-- | Render Markdown as CommonMark text, wrapping paragraphs at the given
-- column.
renderMarkdown :: Int -> Node -> Text
renderMarkdown width = T.unlines . renderBlock width

renderBlock :: Int -> Node -> [Text]
renderBlock width n@(Node typ xs) = case typ of
  DOCUMENT -> joinBlocks (children False)
  PARAGRAPH -> map escapeLineStart (wrap width (renderInlines xs))
  HEADING level -> [T.replicate level "#" <> " " <> T.map unprotect (renderInlines xs)]
  LIST listType -> concat (zipWith (renderItem width) (markers listType) xs)
  -- List items are always tight, so their blocks are not separated by blank
  -- lines.
  ITEM -> concat (children True)
  CODE_BLOCK code -> codeBlock False code
  CUSTOM_BLOCK prefix suffix -> T.lines prefix ++ joinBlocks (children False) ++ T.lines suffix
  _ -> renderBlock width (Node PARAGRAPH [n])
  where
    -- A code block is fenced right after a list marker, where other Markdown
    -- readers most often misread indented code, and right after a list,
    -- where indented code would continue the list's last item.
    children fenceFirst = zipWith child (fenceFirst : map isList xs) xs
    child fence = \case
      Node (CODE_BLOCK code) [] -> codeBlock fence code
      c -> renderBlock width c
    isList = \case
      Node (LIST _) _ -> True
      _ -> False

-- | Render a code block indented, or fenced when asked to or when indented
-- code would drop its leading or trailing blank lines.
codeBlock :: Bool -> Text -> [Text]
codeBlock fence code
  | fence || null ls || any (T.all isSpace) (take 1 ls ++ take 1 (reverse ls)) =
      [fenceLine] ++ ls ++ [fenceLine]
  | otherwise = map indentCode ls
  where
    ls = T.lines code
    fenceLine = T.replicate (max 3 (longestBacktickRun code + 1)) "`"
    indentCode line
      | T.null line = line
      | otherwise = "    " <> line

joinBlocks :: [[Text]] -> [Text]
joinBlocks = \case
  [] -> []
  [b] -> b
  (b : bs) -> b ++ [""] ++ joinBlocks bs

renderItem :: Int -> Text -> Node -> [Text]
renderItem width marker item =
  case renderBlock (width - T.length marker) item of
    [] -> [marker]
    (l : ls) -> (marker <> l) : map indent ls
  where
    indent line
      | T.null line = line
      | otherwise = T.replicate (T.length marker) " " <> line

markers :: ListType -> [Text]
markers = \case
  BulletList -> repeat "  - "
  OrderedList start -> map ordered (iterate (+ 1) start)
  where
    ordered :: Int -> Text
    ordered k =
      let number = T.pack (show k) <> "."
       in number <> T.replicate (max 1 (4 - T.length number)) " "

renderInlines :: [Node] -> Text
renderInlines = T.concat . map renderInline

-- | Render an inline node. Hard breaks become newlines, and spaces that must
-- not become line breaks become 'protectedSpace'; 'wrap' resolves both.
renderInline :: Node -> Text
renderInline (Node typ xs) = case typ of
  TEXT t -> escape t
  EMPH -> "*" <> inner <> "*"
  STRONG -> "**" <> inner <> "**"
  CODE code -> codeSpan code
  LINK url
    | isAutolink url xs -> "<" <> protect url <> ">"
    | otherwise -> "[" <> inner <> "](" <> escapeUrl url <> ")"
  IMAGE url title -> "![" <> inner <> "](" <> escapeUrl url <> imageTitle title <> ")"
  -- prettyDocComment strips trailing whitespace from every line, so a
  -- hard break in the rendered comment is a plain newline.
  LINEBREAK -> "\n"
  CUSTOM_INLINE prefix suffix -> prefix <> inner <> suffix
  _ -> inner
  where
    inner = renderInlines xs
    imageTitle title
      | T.null title = ""
      | otherwise = protect (" \"" <> T.concatMap escapeTitle title <> "\"")
    escapeTitle c
      | c == '"' || c == '\\' = T.pack ['\\', c]
      | otherwise = T.singleton c

-- | Greedily wrap rendered inlines at spaces, starting a new line at every
-- hard break and collapsing runs of spaces.
wrap :: Int -> Text -> [Text]
wrap width = map (T.map unprotect) . concatMap (fill . filter (not . T.null) . T.splitOn " ") . T.splitOn "\n"
  where
    fill = \case
      [] -> []
      (w : ws) -> line (T.length w) [w] ws
    line len acc = \case
      (w : ws)
        | len + 1 + T.length w <= width -> line (len + 1 + T.length w) (w : acc) ws
      ws -> T.unwords (reverse acc) : fill ws

protectedSpace :: Char
protectedSpace = '\0'

protect :: Text -> Text
protect = T.map (\c -> if c == ' ' then protectedSpace else c)

unprotect :: Char -> Char
unprotect c = if c == protectedSpace then ' ' else c

codeSpan :: Text -> Text
codeSpan code = protect (fence <> pad <> content <> pad <> fence)
  where
    content = T.map (\c -> if c == '\n' then ' ' else c) code
    fence = T.replicate (longestBacktickRun content + 1) "`"
    -- CommonMark strips one space from each side of a code span that both
    -- starts and ends with a space.
    pad
      | "`" `T.isPrefixOf` content || "`" `T.isSuffixOf` content = " "
      | " " `T.isPrefixOf` content && " " `T.isSuffixOf` content && T.any (/= ' ') content = " "
      | otherwise = ""

longestBacktickRun :: Text -> Int
longestBacktickRun t = foldr max 0 [T.length run | run <- T.group t, "`" `T.isPrefixOf` run]

isAutolink :: Text -> [Node] -> Bool
isAutolink url = \case
  [Node (TEXT t) []] -> t == url && hasScheme
  _ -> False
  where
    (scheme, rest) = T.break (== ':') url
    hasScheme =
      not (T.null rest)
        && T.length scheme >= 2
        && T.length scheme <= 32
        && T.all (\c -> isAsciiLetter c || isDigit c || c `elem` ("+.-" :: String)) scheme
        && maybe False (isAsciiLetter . fst) (T.uncons scheme)
        && not (T.any (`elem` (" <>" :: String)) url)
    isAsciiLetter c = isAsciiLower c || isAsciiUpper c

escapeUrl :: Text -> Text
escapeUrl = T.concatMap escapeChar
  where
    escapeChar c
      | c == ' ' = "%20"
      | c `elem` ("`<>\\()" :: String) = T.pack ['\\', c]
      | otherwise = T.singleton c

escape :: Text -> Text
escape = T.pack . go . T.unpack
  where
    go = \case
      [] -> []
      (c : cs)
        | c `elem` ("*_[]#<>\\`~!" :: String) || (c == '&' && startsWithLetter cs) -> '\\' : c : go cs
        | otherwise -> c : go cs
    startsWithLetter = \case
      (c : _) -> isAsciiLower c || isAsciiUpper c
      [] -> False

-- | Escape characters that would start a list item, thematic break or setext
-- heading underline at the start of a line.
--
-- This runs after wrapping, on each output line, so wrapping can break lines
-- anywhere without knowing which words would land at the start of a line.
escapeLineStart :: Text -> Text
escapeLineStart line = case T.uncons line of
  Just (c, _) | c `elem` ("-+=" :: String) -> "\\" <> line
  _ ->
    let (digits, rest) = T.span isDigit line
     in case T.uncons rest of
          Just (c, _) | not (T.null digits) && (c == '.' || c == ')') -> digits <> "\\" <> rest
          _ -> line
