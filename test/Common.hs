module Common where

import Moat
import Data.Proxy (Proxy (..))

showKotlin :: forall a. ToMoatData a => String
showKotlin = prettyKotlinData $ toMoatData (Proxy @a)

showSwift :: forall a. ToMoatData a => String
showSwift = prettySwiftData $ toMoatData (Proxy @a)
