{-# LANGUAGE OverloadedStrings #-}

module RenderMarkdownSpec where

import Moat.Pretty.Doc.Markdown
import Moat.Pretty.Doc.Render (renderMarkdown)
import Test.Hspec

spec :: Spec
spec =
  describe "renderMarkdown" $ do
    it "escapes Markdown punctuation in text" $
      renderMarkdown 100 (para [text "*_[]#<>\\`~! &amp; & 1"])
        `shouldBe` "\\*\\_\\[\\]\\#\\<\\>\\\\\\`\\~\\! \\&amp; & 1\n"
    it "escapes list and heading markers that wrapping moves to the start of a line" $
      renderMarkdown 10 (para [text "aaaa bbbb - cc 12345678 1. xxxxxx = y"])
        `shouldBe` "aaaa bbbb\n\\- cc\n12345678\n1\\. xxxxxx\n\\= y\n"
    it "collapses runs of spaces" $
      renderMarkdown 100 (para [text " a   b "])
        `shouldBe` "a b\n"
    it "fences code spans with more backticks than they contain" $
      renderMarkdown 100 (para [node (CODE "a``b") [], text " ", node (CODE "`x") []])
        `shouldBe` "```a``b``` `` `x ``\n"
    it "does not wrap inside code spans or link destinations" $
      renderMarkdown 5 (para [text "aa ", node (CODE "b c d") [], text " ", node (LINK "http://x.y/a b") [text "see it"]])
        `shouldBe` "aa\n`b c d`\n[see\nit](http://x.y/a%20b)\n"
    it "fences a code block that starts a list item and indents other ones" $
      renderMarkdown 100 (document [ul [li [node (CODE_BLOCK "a ```b\n") [], node (CODE_BLOCK "c\n") []]], para [text "e"], node (CODE_BLOCK "d\n") []])
        `shouldBe` "  - ````\n    a ```b\n    ````\n        c\n\ne\n\n    d\n"
    it "fences a code block after a list or with blank edge lines" $
      renderMarkdown 100 (document [ul [li [para [text "a"]]], node (CODE_BLOCK "b\n") [], node (CODE_BLOCK "\nc\n\n") []])
        `shouldBe` "  - a\n\n```\nb\n```\n\n```\n\nc\n\n```\n"
    it "escapes quotes and backslashes in image titles" $
      renderMarkdown 100 (para [node (IMAGE "u" "a \"b\" c\\") [text "alt"]])
        `shouldBe` "![alt](u \"a \\\"b\\\" c\\\\\")\n"
