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

prettyFieldDoc :: Int -> [Field] -> Maybe String
prettyFieldDoc wrap fields =
  let md = case foldr joinParams [] (mapMaybe fieldParam fields) of
        [] -> Nothing
        blocks -> Just (document blocks)
   in T.unpack . renderMarkdown wrap <$> md

fieldParam :: Field -> Maybe [Node]
fieldParam Field {fieldDoc = Nothing} = Nothing
fieldParam Field {fieldName = name, fieldDoc = Just doc} =
  Just $ prependInlines [text ("@param " ++ name ++ " ")] (markdown text identifier (parseDoc doc))

-- | Join the blocks of two params. A paragraph that ends the first and the
-- paragraph that starts the second become one paragraph with a hard break
-- between them, so each @\@param@ starts a line without a blank line before
-- it.
joinParams :: [Node] -> [Node] -> [Node]
joinParams a b = case (reverse a, b) of
  (Node PARAGRAPH xs : before, Node PARAGRAPH ys : after) ->
    reverse before ++ Node PARAGRAPH (xs ++ br : ys) : after
  _ -> a ++ b

identifier :: String -> Node
identifier ident = inline "[" "]" [text ident]
