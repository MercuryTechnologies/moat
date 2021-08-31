module Common where

import Data.Proxy (Proxy (..))
import Moat

showKotlin :: forall a. ToMoatData a => String
showKotlin = prettyKotlinData $ toMoatData (Proxy @a)

showSwift :: forall a. ToMoatData a => String
showSwift = prettySwiftData $ toMoatData (Proxy @a)

showTypescript :: forall a. ToMoatData a => String
showTypescript = prettyTypescriptData $ toMoatData (Proxy @a)
