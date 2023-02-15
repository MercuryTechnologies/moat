{-# LANGUAGE ScopedTypeVariables #-}
module Moat.Pretty.Doc.Markdown
  ( block,
    br,
    document,
    inline,
    inlineContent,
    li,
    markdown,
    node,
    ol,
    para,
    parseDoc,
    text,
    ul,
  )
where

import CMarkGFM
import Documentation.Haddock.Markup
import Documentation.Haddock.Parser
import Documentation.Haddock.Types
import qualified Data.Text as T

-- | Parse a Haddock comment.
--
-- See 'markdown' to generate Markdown nodes from this value.
parseDoc :: String -> DocH m String
parseDoc docstr = toRegular $ _doc $ parseParas Nothing docstr

-- | Translate Haddock documentation into Markdown nodes.
markdown ::
  -- | Function to format a module name as a Markdown node.
  (String -> Node) ->
  -- | Function to format an identifier as a Markdown node.
  (String -> Node) ->
  -- | Incoming Haddock documentation.
  DocH String String
  -> Node
markdown m i doc = document (markdownBlocks m i doc)

markdownBlocks ::
  (String -> Node) ->
  (String -> Node) ->
  DocH String String ->
  [Node]
markdownBlocks m i = \case
  DocAppend a b -> markdownBlocks m i a ++ markdownBlocks m i b
  DocParagraph d -> [node PARAGRAPH (markdownInlines m i d)]
  DocUnorderedList ds -> [ul (map (markdownListItem m i) ds)]
  DocOrderedList ds -> [ol (fst (head ds)) (map (markdownListItem m i . snd) ds)]
  DocCodeBlock d -> [node (CODE_BLOCK T.empty (T.pack (markupText d))) []]
  DocHeader (Header l t) -> [node (HEADING l) (markdownInlines m i t)]
  _ -> []

markdownListItem :: (String -> Node) -> (String -> Node) -> DocH String String -> Node
markdownListItem m i doc = node ITEM (markdownBlocks m i doc)

markdownInlines :: (String -> Node) -> (String -> Node) -> DocH String String -> [Node]
markdownInlines m i = \case
  DocAppend a b -> markdownInlines m i a ++ markdownInlines m i b
  DocString s -> [text s]
  DocIdentifier x -> [i x]
  DocIdentifierUnchecked x -> [node (CODE (T.pack x)) []]
  DocModule (ModLink mo l) -> [link m i mo l]
  DocEmphasis d -> [node EMPH (markdownInlines m i d)]
  DocBold d -> [node STRONG (markdownInlines m i d)]
  DocMonospaced d -> [node (CODE (T.pack (markupText d))) []]
  DocHyperlink (Hyperlink u l) -> [link m i u l]
  DocPic (Picture u t) -> [node (IMAGE (T.pack u) (maybe T.empty T.pack t)) []]
  _ -> []

markupText :: DocH String String -> String
markupText = markup $ plainMarkup id id

-- | Create a Markdown node with a node type and children.
node :: NodeType -> [Node] -> Node
node = Node Nothing

-- | Create a Markdown document node.
document :: [Node] -> Node
document = node DOCUMENT

-- | Create a Markdown text node.
text :: String -> Node
text s = node (TEXT (T.pack (filterNewlines s))) []
  where
    filterNewlines = filter (/= '\n')

-- | Create a Markdown linebreak node.
br :: Node
br = node LINEBREAK []

-- | Create a Markdown paragraph node.
para :: [Node] -> Node
para = node PARAGRAPH

-- | Create a custom inline Markdown node, with prefix and suffix
-- literal strings and inline or block child nodes.
block :: String -> String -> [Node] -> Node
block prefix suffix = node (CUSTOM_BLOCK (T.pack prefix) (T.pack suffix))

-- | Create a custom inline Markdown node, with prefix and suffix
-- literal strings and inline child nodes.
inline :: String -> String -> [Node] -> Node
inline prefix suffix = node (CUSTOM_INLINE (T.pack prefix) (T.pack suffix))

ul :: [Node] -> Node
ul = node (LIST (ListAttributes {listType = BULLET_LIST, listTight = True, listStart = 1, listDelim = PERIOD_DELIM}))

ol :: Int -> [Node] -> Node
ol i = node (LIST (ListAttributes {listType = ORDERED_LIST, listTight = True, listStart = i, listDelim = PERIOD_DELIM}))

li :: [Node] -> Node
li = node ITEM

link :: (String -> Node) -> (String -> Node) -> String -> Maybe (DocH String String) -> Node
link m i url lbl = node (LINK (T.pack url) T.empty) (maybe [text url] (markdownInlines m i) lbl)

inlineContent :: Node -> [Node]
inlineContent n@(Node _ typ xs) =
  if isBlock typ
  then concatMap inlineContent xs
  else [n]

isBlock :: NodeType -> Bool
isBlock = \case
  DOCUMENT -> True
  BLOCK_QUOTE -> True
  (LIST _) -> True
  ITEM -> True
  (CODE_BLOCK _ _) -> True
  (HTML_BLOCK _) -> True
  (CUSTOM_BLOCK _ _) -> True
  PARAGRAPH -> True
  (HEADING _) -> True
  THEMATIC_BREAK -> True
  FOOTNOTE_DEFINITION -> True
  _ -> False
