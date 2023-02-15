{-# LANGUAGE CPP #-}
module Common where

import Data.Proxy (Proxy (..))
import Moat

showKotlin :: forall a. ToMoatData a => String
showKotlin = prettyKotlinData $ toMoatData (Proxy @a)

showSwift :: forall a. ToMoatData a => String
showSwift = prettySwiftData $ toMoatData (Proxy @a)

hasDoc :: Bool
#if MIN_VERSION_template_haskell(2,18,0)
hasDoc = True
#else
hasDoc = False
#endif
