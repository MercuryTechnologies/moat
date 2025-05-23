{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Moat.Class
  ( ToMoatType (..),
    ToMoatData (..),
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.CaseInsensitive (CI)
import Data.Fixed (Fixed, HasResolution)
import qualified Data.HashMap.Strict as HM
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as M
import qualified Data.Primitive as Prim
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime)
import Data.UUID.Types (UUID)
import Data.Vector (Vector)
import Data.Word (Word16, Word32, Word64, Word8)
import Moat.Types
import Prelude hiding (Enum (..))

-- | The class for things which can be converted to
--   'MoatData'.
--
--   Typically the instance will be generated by
--   'mobileGen', 'mobileGenWith'.
class ToMoatData a where
  -- | Convert a Proxy type to 'MoatData'
  toMoatData :: Proxy a -> MoatData

-- | The class for things which can be converted to
--   a 'MoatType'.
--
--   Typically the instance will be generated by
--   'mobileGen', 'mobileGenWith'.
class ToMoatType a where
  -- | Convert a Proxy of type to a 'MoatType'.
  toMoatType :: Proxy a -> MoatType

instance ToMoatType () where
  toMoatType _ = Unit

instance ToMoatType Bool where
  toMoatType _ = Bool

instance ToMoatType UUID where
  toMoatType _ = Concrete "UUID" []

instance ToMoatType UTCTime where
  toMoatType _ = Concrete "Date" []

instance forall a b. (ToMoatType a, ToMoatType b) => ToMoatType (a -> b) where
  toMoatType _ = App (toMoatType (Proxy @a)) (toMoatType (Proxy @b))

instance forall a. ToMoatType a => ToMoatType (Maybe a) where
  toMoatType _ = Optional (toMoatType (Proxy @a))

instance forall a b. (ToMoatType a, ToMoatType b) => ToMoatType (Either a b) where
  toMoatType _ = Result (toMoatType (Proxy @a)) (toMoatType (Proxy @b))

instance ToMoatType Integer where
  toMoatType _ = BigInt

instance ToMoatType Int where toMoatType _ = I

instance ToMoatType Int8 where toMoatType _ = I8

instance ToMoatType Int16 where toMoatType _ = I16

instance ToMoatType Int32 where toMoatType _ = I32

instance ToMoatType Int64 where toMoatType _ = I64

instance ToMoatType Word where toMoatType _ = U

instance ToMoatType Word8 where toMoatType _ = U8

instance ToMoatType Word16 where toMoatType _ = U16

instance ToMoatType Word32 where toMoatType _ = U32

instance ToMoatType Word64 where toMoatType _ = U64

instance ToMoatType Float where toMoatType _ = F32

instance ToMoatType Double where toMoatType _ = F64

instance ToMoatType Char where toMoatType _ = Character

instance HasResolution a => ToMoatType (Fixed a) where
  toMoatType _ = Decimal

instance forall a. (ToMoatType a) => ToMoatType (Prim.Array a) where
  toMoatType _ = Array (toMoatType (Proxy @a))

instance forall a. (ToMoatType a) => ToMoatType (Prim.SmallArray a) where
  toMoatType _ = Array (toMoatType (Proxy @a))

instance ToMoatType Prim.ByteArray where
  toMoatType _ = Array U8

instance forall a. (ToMoatType a) => ToMoatType (Prim.PrimArray a) where
  toMoatType _ = Array (toMoatType (Proxy @a))

instance forall a. ToMoatType a => ToMoatType (Vector a) where
  toMoatType _ = Array (toMoatType (Proxy @a))

instance forall a. ToMoatType a => ToMoatType (NonEmpty a) where
  toMoatType _ = Array (toMoatType (Proxy @a))

instance {-# OVERLAPPABLE #-} forall a. ToMoatType a => ToMoatType [a] where
  toMoatType _ = Array (toMoatType (Proxy @a))

instance {-# OVERLAPPING #-} ToMoatType [Char] where toMoatType _ = Str

instance forall a. ToMoatType a => ToMoatType (Set a) where
  toMoatType _ = Set (toMoatType (Proxy @a))

instance ToMoatType TL.Text where toMoatType _ = Str

instance ToMoatType TS.Text where toMoatType _ = Str

instance ToMoatType BL.ByteString where toMoatType _ = Array U8

instance ToMoatType BS.ByteString where toMoatType _ = Array U8

instance ToMoatType (CI s) where toMoatType _ = Str

instance forall k v. (ToMoatType k, ToMoatType v) => ToMoatType (M.Map k v) where
  toMoatType _ = Dictionary (toMoatType (Proxy @k)) (toMoatType (Proxy @v))

instance forall k v. (ToMoatType k, ToMoatType v) => ToMoatType (HM.HashMap k v) where
  toMoatType _ = Dictionary (toMoatType (Proxy @k)) (toMoatType (Proxy @v))

instance forall a b. (ToMoatType a, ToMoatType b) => ToMoatType ((,) a b) where
  toMoatType _ = Tuple2 (toMoatType (Proxy @a)) (toMoatType (Proxy @b))

instance forall a b c. (ToMoatType a, ToMoatType b, ToMoatType c) => ToMoatType ((,,) a b c) where
  toMoatType _ = Tuple3 (toMoatType (Proxy @a)) (toMoatType (Proxy @b)) (toMoatType (Proxy @c))
