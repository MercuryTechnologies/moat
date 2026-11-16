{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moat.Pretty.Doc.Markdown
  ( ListType (..),
    Node (..),
    NodeType (..),
    block,
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

import Data.Text (Text)
import qualified Data.Text as T
import Documentation.Haddock.Markup
import Documentation.Haddock.Parser
import Documentation.Haddock.Types

-- | A Markdown node: a node type and its children.
data Node = Node NodeType [Node]
  deriving (Eq)

-- | The Markdown constructs moat generates from Haddock.
data NodeType
  = DOCUMENT
  | PARAGRAPH
  | HEADING Int
  | LIST ListType
  | ITEM
  | -- | Code block with its literal contents.
    CODE_BLOCK Text
  | TEXT Text
  | EMPH
  | STRONG
  | CODE Text
  | -- | Link with its destination URL.
    LINK Text
  | -- | Image with its URL and title.
    IMAGE Text Text
  | LINEBREAK
  | -- | Inline with literal prefix and suffix.
    CUSTOM_INLINE Text Text
  | -- | Block with literal prefix and suffix.
    CUSTOM_BLOCK Text Text
  deriving (Eq)

-- | A tight list, either bulleted or numbered from a start number.
data ListType
  = BulletList
  | OrderedList Int
  deriving (Eq)

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
  DocH String String ->
  Node
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
  DocOrderedList ds -> [markdownOrderedList m i ds]
  DocCodeBlock d -> [node (CODE_BLOCK (T.pack (markupText d))) []]
  DocHeader (Header l t) -> [node (HEADING l) (markdownInlines m i t)]
  _ -> []

#if MIN_VERSION_haddock_library(1,11,0)
markdownOrderedList :: (String -> Node) -> (String -> Node) -> [(Int, DocH String String)] -> Node
markdownOrderedList m i ds = ol start (map (markdownListItem m i . snd) ds)
  where
    start = case ds of
      ((n, _) : _) -> n
      [] -> 1
#else
markdownOrderedList :: (String -> Node) -> (String -> Node) -> [DocH String String] -> Node
markdownOrderedList m i ds = ol 1 (map (markdownListItem m i) ds)
#endif

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
node = Node

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
ul = node (LIST BulletList)

ol :: Int -> [Node] -> Node
ol i = node (LIST (OrderedList i))

li :: [Node] -> Node
li = node ITEM

link :: (String -> Node) -> (String -> Node) -> String -> Maybe (DocH String String) -> Node
link m i url lbl = node (LINK (T.pack url)) (maybe [text url] (markdownInlines m i) lbl)

inlineContent :: Node -> [Node]
inlineContent n@(Node typ xs) =
  if isBlock typ
    then concatMap inlineContent xs
    else [n]

isBlock :: NodeType -> Bool
isBlock = \case
  DOCUMENT -> True
  (LIST _) -> True
  ITEM -> True
  (CODE_BLOCK _) -> True
  (CUSTOM_BLOCK _ _) -> True
  PARAGRAPH -> True
  (HEADING _) -> True
  _ -> False
