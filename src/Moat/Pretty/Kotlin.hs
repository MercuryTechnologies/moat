module Moat.Pretty.Kotlin
  ( prettyKotlinData,
    EncodingStyle (..),
  )
where

import qualified Moat.Pretty.Kotlin.TaggedFlatObject as TaggedFlatObject
import qualified Moat.Pretty.Kotlin.TaggedObject as TaggedObject
import Moat.Types

-- Aeson provides a few different encoding styles, https://hackage.haskell.org/package/aeson-2.0.1.0/docs/Data-Aeson-TH.html#t:SumEncoding
-- The original style we implemented will be called 'TaggedFlatObject' but isn't available in Aeson yet: https://github.com/haskell/aeson/pull/828
--
-- The 'TaggedObject' style will encode a sum of products where the parent sum has
-- a tag field and a contents field.
--
-- The 'TaggedFlatObject' style will encode a sum of products where the parent sum
-- has only a tag field.
data EncodingStyle
  = TaggedObjectStyle TaggedObject.TaggedObject
  | TaggedFlatObjectStyle TaggedFlatObject.TaggedFlatObject

prettyKotlinData :: EncodingStyle -> MoatData -> String
prettyKotlinData (TaggedObjectStyle to) = TaggedObject.prettyKotlinData to
prettyKotlinData (TaggedFlatObjectStyle tfo) = TaggedFlatObject.prettyKotlinData tfo
