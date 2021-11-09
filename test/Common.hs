module Common where

import Data.Proxy (Proxy (..))
import Moat

showKotlinTaggedObject :: forall a. ToMoatData a => String
showKotlinTaggedObject =
  prettyKotlinData
    (TaggedObjectStyle $ TaggedObject "tag" "contents")
    (toMoatData $ Proxy @a)

showKotlinTaggedFlatObject :: forall a. ToMoatData a => String
showKotlinTaggedFlatObject =
  prettyKotlinData
    (TaggedFlatObjectStyle $ TaggedFlatObject "tag")
    (toMoatData $ Proxy @a)

showSwift :: forall a. ToMoatData a => String
showSwift = prettySwiftData $ toMoatData (Proxy @a)
