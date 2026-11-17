{-# LANGUAGE OverloadedStrings #-}

module Moat.Pretty.Doc.KDoc
  ( prettyDoc,
    prettyDocComment,
    prettyFieldDoc,
  )
where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Moat.Pretty.Doc.Markdown
import Moat.Pretty.Doc.Render (renderMarkdown)
import Moat.Types (Field (..))

prettyDocComment :: Int -> String -> String -> String
prettyDocComment wrap indents str =
  let ls = lines str
   in case ls of
        [] -> ""
        [l] -> if length l + length indents < wrap then short l else long ls
        _ -> long ls
  where
    short :: String -> String
    short l = indents ++ "/** " ++ l ++ " */\n"

    long :: [String] -> String
    long ls =
      indents
        ++ "/**\n"
        ++ concatMap (\s -> dropWhileEnd isSpace (indents ++ " * " ++ s) ++ "\n") ls
        ++ indents
        ++ " */\n"

prettyDoc :: Int -> String -> String
prettyDoc wrap haddock =
  let md = markdown text identifier (parseDoc haddock)
      docstr = renderMarkdown wrap md
   in T.unpack docstr

-- | KDoc splits block tags at line starts before parsing Markdown, so each
-- @\@param@ follows the previous one without a blank line, even after a list.
prettyFieldDoc :: Int -> [Field] -> Maybe String
prettyFieldDoc wrap fields = case mapMaybe fieldParam fields of
  [] -> Nothing
  params -> Just (concatMap (T.unpack . renderMarkdown wrap . document) params)

fieldParam :: Field -> Maybe [Node]
fieldParam Field {fieldDoc = Nothing} = Nothing
fieldParam Field {fieldName = name, fieldDoc = Just doc} =
  Just $ prependInlines [text ("@param " ++ name ++ " ")] (escapeTagStarts (markdown text identifier (parseDoc doc)))

-- | Escape an @\@@ that starts a paragraph. KDoc reads a line that starts with
-- @\@word@ as a block tag, which would end the @\@param@ description.
escapeTagStarts :: Node -> Node
escapeTagStarts (Node typ xs) = case (typ, xs) of
  (PARAGRAPH, Node (TEXT t) [] : _)
    | "@" `T.isPrefixOf` t -> Node PARAGRAPH (inline "\\" "" [] : xs)
  (PARAGRAPH, _) -> Node PARAGRAPH xs
  _ -> Node typ (map escapeTagStarts xs)

identifier :: String -> Node
identifier ident = inline "[" "]" [text ident]
