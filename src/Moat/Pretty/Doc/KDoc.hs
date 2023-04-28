{-# LANGUAGE OverloadedStrings #-}

module Moat.Pretty.Doc.KDoc
  ( prettyDoc,
    prettyDocComment,
    prettyFieldDoc,
  )
where

import CMarkGFM (Node (..), nodeToCommonmark)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate)
import qualified Data.Text as T
import Moat.Pretty.Doc.Markdown
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
  let cmark = markdown text identifier (parseDoc haddock)
      docstr = nodeToCommonmark [] (Just wrap) cmark
   in T.unpack docstr

prettyFieldDoc :: Int -> [Field] -> Maybe String
prettyFieldDoc wrap fields =
  let params = filter (/= []) (map fieldParam fields)
      cmark = case params of
        [] -> Nothing
        _ -> Just $ para (intercalate [br] params)
   in T.unpack . nodeToCommonmark [] (Just wrap) <$> cmark

fieldParam :: Field -> [Node]
fieldParam Field {fieldDoc = Nothing} = []
fieldParam Field {fieldDoc = Just doc, ..} =
  text ("@param " ++ fieldName ++ " ") : inlineContent (markdown text identifier (parseDoc doc))

identifier :: String -> Node
identifier ident = inline "[" "]" [text ident]
