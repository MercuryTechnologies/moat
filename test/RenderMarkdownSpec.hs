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
      renderMarkdown 100 (document [ul [li [node (CODE_BLOCK "a ```b\n") [], node (CODE_BLOCK "cc\n") []]], para [text "e"], node (CODE_BLOCK "dd\n") []])
        `shouldBe` "  - ````\n    a ```b\n    ````\n        cc\n\ne\n\n    dd\n"
    it "ends a list followed by a code block, and fences code with blank edge lines" $
      renderMarkdown 100 (document [ul [li [para [text "a"]]], node (CODE_BLOCK "bb\n") [], node (CODE_BLOCK "\nc\n\n") []])
        `shouldBe` "  - a\n\n<!-- end list -->\n\n    bb\n\n```\n\nc\n\n```\n"
    it "ends a list followed by another list" $
      renderMarkdown 100 (document [ul [li [para [text "a"]]], ul [li [para [text "b"]]], ol 5 [li [para [text "c"]]]])
        `shouldBe` "  - a\n\n<!-- end list -->\n\n  - b\n\n<!-- end list -->\n\n5.  c\n"
    it "ends a list followed by another list inside a list item" $
      renderMarkdown 100 (document [ul [li [para [text "a"], ol 1 [li [para [text "one"]]], ol 1 [li [para [text "again"]]]]]])
        `shouldBe` "  - a\n    1.  one\n    <!-- end list -->\n    1.  again\n"
    it "fences code blocks of at most two bytes or with a leading space" $
      renderMarkdown 100 (document [node (CODE_BLOCK "x\n") [], node (CODE_BLOCK " a\n") [], node (CODE_BLOCK "ab\n") [], node (CODE_BLOCK "\233\n") []])
        `shouldBe` "```\nx\n```\n\n```\n a\n```\n\n    ab\n\n    \233\n"
    it "writes control characters as entities" $
      renderMarkdown 100 (para [text "a\tb\fc"])
        `shouldBe` "a&#9;b&#12;c\n"
    it "escapes quotes and backslashes in image titles" $
      renderMarkdown 100 (para [node (IMAGE "u" "a \"b\" c\\") [text "alt"]])
        `shouldBe` "![alt](u \"a \\\"b\\\" c\\\\\")\n"
