{-# LANGUAGE OverloadedStrings #-}

module Moat.Pretty.Doc.DocC
  ( prettyDoc,
    prettyDocComment,
    prettyFieldDoc
  )
where

import Moat.Pretty.Doc.Markdown
import Moat.Types (Field (..))
import CMarkGFM (nodeToCommonmark, Node (..))
import qualified Data.Text as T
import Data.Maybe (mapMaybe)

prettyDocComment :: String -> String -> String
prettyDocComment indents str = concatMap (\s -> indents ++ "/// " ++ s ++ "\n") (lines str)

-- | Format Haddock documentation as Markdown, wrapping at the
-- given column.
prettyDoc :: Int -> String -> String
prettyDoc wrap haddock =
  let
    cmark = markdown text identifier (parseDoc haddock)
    docstr = nodeToCommonmark [] (Just wrap) cmark
  in T.unpack docstr

prettyFieldDoc :: Int -> [Field] -> Maybe String
prettyFieldDoc wrap fields =
  let
    items = mapMaybe paramDoc fields
    cmark = case items of
      [] -> Nothing
      _ -> Just $
             ul [li [para [text "Parameters:"], ul items]]
  in T.unpack . nodeToCommonmark [] (Just wrap) <$> cmark

paramDoc :: Field -> Maybe Node
paramDoc Field { fieldDoc = Nothing } = Nothing
paramDoc Field { fieldName = name, fieldDoc = Just doc } =
  let
    prefix = name ++ ": "
    cmark = markdown text identifier (parseDoc doc)
  in Just $ li [ para (text prefix : inlineContent cmark) ]

identifier :: String -> Node
identifier ident = inline "``" "``" [text ident]
