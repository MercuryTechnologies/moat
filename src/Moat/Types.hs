{-# language
    AllowAmbiguousTypes
  , DeriveGeneric
  , DeriveLift
  , DerivingStrategies
#-}

module Moat.Types
  ( MoatType(..)
  , MoatData(..)
  , Backend(..)
  , Protocol(..)
  , Interface(..)
  , Options(..)
  , KeepOrDiscard(..)
  , Annotation(..)
  , defaultOptions
  ) where

import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)

-- | An AST representing a type.
data MoatType
  = Unit
    -- ^ Unit (called "Unit/Void" in swift). Empty struct type.
  | Bool
    -- ^ Bool
  | Character
    -- ^ Character
  | Str
    -- ^ String. Named 'Str' to avoid conflicts with
    --   'Data.Aeson.String'.
  | I
    -- ^ signed machine integer
  | I8
    -- ^ signed 8-bit integer
  | I16
    -- ^ signed 16-bit integer
  | I32
    -- ^ signed 32-bit integer
  | I64
    -- ^ signed 64-bit integer
  | U
    -- ^ unsigned machine integer
  | U8
    -- ^ unsigned 8-bit integer
  | U16
    -- ^ unsigned 16-bit integer
  | U32
    -- ^ unsigned 32-bit integer
  | U64
    -- ^ unsigned 64-bit integer
  | F32
    -- ^ 32-bit floating point
  | F64
    -- ^ 64-bit floating point
  | Decimal
    -- ^ Increased-precision floating point
  | BigInt
    -- ^ 64-bit big integer
  | Tuple2 MoatType MoatType
    -- ^ 2-tuple
  | Tuple3 MoatType MoatType MoatType
    -- ^ 3-tuple
  | Optional MoatType
    -- ^ Maybe type
  | Result MoatType MoatType
    -- ^ Either type
    --
    --   /Note/: The error type in Swift must
    --   implement the @Error@ protocol. This library
    --   currently does not enforce this.
  | Set MoatType
    -- ^ Set type
  | Dictionary MoatType MoatType
    -- ^ Dictionary type
  | Array MoatType
    -- ^ array type
  | App MoatType MoatType
    -- ^ function type
  | Poly String
    -- ^ polymorphic type variable
  | Concrete
      { concreteName :: String
        -- ^ the name of the type
      , concreteTyVars :: [MoatType]
        -- ^ the type's type variables
      }
    -- ^ a concrete type variable, and its
    --   type variables. Will typically be generated
    --   by 'getShwifty'.
  | Tag
      { tagName :: String
        -- ^ the name of the type
      , tagParent :: String
        -- ^ the type constructor of the type
        --   to which this alias belongs
      , tagTyp :: MoatType
        -- ^ the type that this represents
      , tagDisambiguate :: Bool
        -- ^ does the type need disambiguation?
        --
        --   This will happen if there are multiple
        --   tags with the same type. This is needed
        --   to maintain safety.
      }
    -- ^ A @Tagged@ typealias, for newtyping
    --   in a way that doesn't break Codable.
    --
    --   See 'getShwiftyWithTags' for examples.
  deriving stock (Eq, Show, Read)
  deriving stock (Generic)
  deriving stock (Lift)

-- | A datatype, either a struct (product type),
--   enum (sum type), or type alias. Haskll types are
--   sums-of-products, so the way we differentiate
--   when doing codegen,
--   is that types with a single constructor
--   will be converted to a struct, and those with
--   two or more constructors will be converted to an
--   enum. Types with 0 constructors will be converted
--   to an empty enum.
data MoatData
  = MoatStruct
      { structName :: String
        -- ^ The name of the struct
      , structTyVars :: [String]
        -- ^ The struct's type variables
      , structInterfaces :: [Interface]
        -- ^ The interfaces which the struct implements
      , structProtocols :: [Protocol]
        -- ^ The protocols which the struct implements
      , structAnnotations :: [Annotation]
        -- ^ The annotations on the struct
      , structFields :: [(String, MoatType)]
        -- ^ The fields of the struct. the pair
        --   is interpreted as (name, type).
      , structPrivateTypes :: [MoatData]
        -- ^ Private types of the struct. Typically
        --   populated by setting 'makeBase'.
        --
        --   Only used by the Swift backend.
      , structTags :: [MoatType]
        -- ^ The tags of the struct. See 'Tag'.
        --
        --   Only used by the Swift backend.
      }
    -- ^ A struct (product type)
  | MoatEnum
      { enumName :: String
        -- ^ The name of the enum
      , enumTyVars :: [String]
        -- ^ The enum's type variables
      , enumInterfaces :: [Interface]
        -- ^ The interfaces (Kotlin) which the enum implements
      , enumProtocols :: [Protocol]
        -- ^ The interfaces (Kotlin) which the enum implements
      , enumAnnotations :: [Annotation]
        -- ^ The annotations on the enum
      , enumCases :: [(String, [(Maybe String, MoatType)])]
        -- ^ The cases of the enum. the type
        --   can be interpreted as
        --   (name, [(label, type)]).
      , enumRawValue :: Maybe MoatType
        -- ^ The rawValue of an enum. See
        --   https://developer.apple.com/documentation/swift/rawrepresentable/1540698-rawvalue
        --
        --   Typically the 'Ty' will be
        --   'I' or 'String'.
        --
        --   /Note/: Currently, nothing will prevent
        --   you from putting something
        --   nonsensical here.
        --
        --   Only used by the Swift backend.
      , enumPrivateTypes :: [MoatData]
        -- ^ Private types of the enum. Typically
        --   populated by setting 'makeBase'.
        --
        --   Only used by the Swift backend.
      , enumTags :: [MoatType]
        -- ^ The tags of the struct. See 'Tag'.
        --
        --   Only used by the Swift backend.
      }
    -- ^ An enum (sum type)
  | MoatNewtype
      { newtypeName :: String
      , newtypeTyVars :: [String]
      , newtypeField :: (String, MoatType)
      , newtypeInterfaces :: [Interface]
      , newtypeProtocols :: [Protocol] -- TODO: remove this?
      , newtypeAnnotations :: [Annotation]
      }
    -- ^ A newtype.
    --   Kotlin backend: becomes an inline class.
    --   Swift backend: Becomes an empty enum with a tag.
  | MoatAlias
      { aliasName :: String
        -- ^ the name of the type alias
      , aliasTyVars :: [String]
        -- ^ the type variables of the type alias
      , aliasTyp :: MoatType
        -- ^ the type this represents (RHS)
      }
    -- ^ A /top-level/ type alias
  deriving stock (Eq, Read, Show, Generic)

data Backend
  = Kotlin
  | Swift
  deriving stock (Eq, Read, Show)

data Interface
  = Parcelable
  | RawInterface String
  -- ^ Derive from some Kotlin interface
  -- @
  --     data class A(
  --       ...
  --     ) : {String}
  -- @
  | LinkEnumInterface String
  -- ^ Derive from a sealed class
  -- @
  --     data class A(
  --       ...
  --     ) : {String}()
  -- @
  deriving stock (Eq, Read, Show)
  deriving stock (Lift)

data Annotation
  = Parcelize
  | Serializable
  | RawAnnotation String
  deriving stock (Eq, Read, Show)
  deriving stock (Lift)

-- | Swift protocols.
--   Only a few are supported right now.
data Protocol
  = Hashable
    -- ^ The 'Hashable' protocol.
    --   See https://developer.apple.com/documentation/swift/hashable
  | Codable
    -- ^ The 'Codable' protocol.
    --   See https://developer.apple.com/documentation/swift/codable
  | Equatable
    -- ^ The 'Equatable' protocol.
    --   See https://developer.apple.com/documentation/swift/equatable
  | OtherProtocol String
    -- ^ A user-specified protocol.
  deriving stock (Eq, Read, Show, Generic)
  deriving stock (Lift)

-- | Options that specify how to
--   encode your 'MoatData' to a concrete type.
--
--   Options can be set using record syntax on
--   'defaultOptions' with the fields below.
data Options = Options
  { typeConstructorModifier :: String -> String
    -- ^ Function applied to type constructor names.
    --   The default ('id') makes no changes.
  , fieldLabelModifier :: String -> String
    -- ^ Function applied to field labels.
    --   Handy for removing common record prefixes,
    --   for example. The default ('id') makes no
    --   changes.
  , constructorModifier :: String -> String
    -- ^ Function applied to value constructor names.
    --   The default ('id') makes no changes.
  , optionalExpand :: Bool
    -- ^ Whether or not to truncate Optional types.
    --   Normally, an Optional ('Maybe') is encoded
    --   as "A?", which is syntactic sugar for
    --   "Optional\<A\>". The default value ('False')
    --   will keep it as sugar. A value of 'True'
    --   will expand it to be desugared.
  , generateToMoatType :: Bool
    -- ^ Whether or not to generate a 'ToMoatType'
    --   instance. Sometimes this can be desirable
    --   if you want to define the instance by hand,
    --   or the instance exists elsewhere.
    --   The default is 'True', i.e., to generate
    --   the instance.
  , generateToMoatData :: Bool
    -- ^ Whether or not to generate a 'ToSwiftData'
    --   instance. Sometime this can be desirable
    --   if you want to define the instance by hand,
    --   or the instance exists elsewhere.
    --   The default is 'True', i.e., to generate
    --   the instance.
  , dataInterfaces :: [Interface]
    -- ^ Interfaces to add to a type.
    --   The default (@[]@) will add none.
    --
    --   This is only meaningful on the Kotlin
    --   backend.
  , dataProtocols :: [Protocol]
    -- ^ Protocols to add to a type.
    --   The default (@[]@) will add none.
    --
    --   This is only meaningful on the Swift
    --   backend.
  , dataAnnotations :: [Annotation]
    -- ^ Annotations to add to a type.
    --   The default (@[]@) will add none.
    --
    --   This is only meaningful on the Kotlin
    --   backend.
  , dataRawValue :: Maybe MoatType
    -- ^ The rawValue of an enum. See
    --   https://developer.apple.com/documentation/swift/rawrepresentable/1540698-rawvalue
    --
    --   The default ('Nothing') will not
    --   include any rawValue.
    --
    --   Typically, if the type does have
    --   a 'rawValue', the 'Ty' will be
    --   'I' or 'Str'.
    --
    --   /Note/: Currently, nothing will prevent
    --   you from putting something
    --   nonsensical here.
    --
    --   This is only meaningful on the Swift
    --   backend.
  , typeAlias :: Bool
    -- ^ Whether or not to generate a newtype as
    --   a type alias. Consider if you want this
    --   or to use 'getShwiftyWithTags' instead.
    --
    --   The default ('False') will generate newtypes
    --   as their own structs.
  , newtypeTag :: Bool
    -- ^ Whether or not to generate a newtype as an
    --   empty enum with a tag. This is for type
    --   safety reasons, but with retaining the
    --   ability to have Codable conformance.
    --
    --   The default ('False') will not do this.
    --
    --   /Note/: This takes priority over 'typeAlias'.
    --
    --   /Note/: This option is not currently
    --   supported for newtype instances.
    --
    --   This is only meaningful on the Swift
    --   backend.
    --
    -- === __Examples__
    --
    -- > newtype NonEmptyText = MkNonEmptyText String
    -- > $(getShwiftyWith (defaultOptions { newtypeTag = True }) ''NonEmpyText)
    --
    -- @
    -- enum NonEmptyTextTag {
    --     typealias NonEmptyText = Tagged\<NonEmptyTextTag, String\>
    -- }
    -- @
  , lowerFirstField :: Bool
    -- ^ Whether or not to lower-case the first
    --   character of a field after applying all
    --   modifiers to it.
    --
    --   The default ('True') will do so.
  , lowerFirstCase :: Bool
    -- ^ Whether or not to lower-case the first
    --   character of a case after applying all
    --   modifiers to it.
    --
    --   The default ('True') will do so.
  , omitFields :: String -> KeepOrDiscard
    -- ^ Fields to omit from a struct when
    --   generating types.
    --
    --   The default (@[]@) will omit nothing.
  , omitCases :: String -> KeepOrDiscard
    -- ^ Cases to omit from an enum when
    --   generating types.
    --
    --   The default (@[]@) will omit nothing.
  , makeBase :: (Bool, Maybe MoatType, [Protocol])
    -- ^ Whether or not to make a base type,
    --   its raw value, and its protocols.
    --
    --   Here, "base type" refers to a
    --   version of the type without any fields.
    --   This can be useful for doing Codable
    --   conversions.
    --
    --   The default ('False', 'Nothing', @[]@)
    --   will not create the base type.
    --
    --   This option is only meaningful on the
    --   Swift backend.
  }

-- | The default 'Options'.
--
-- @
-- defaultOptions :: Options
-- defaultOptions = Options
--   { typeConstructorModifier = id
--   , fieldLabelModifier = id
--   , constructorModifier = id
--   , optionalExpand = False
--   , generateToMoatType = True
--   , generateToMoatData = True
--   , dataInterfaces = []
--   , dataProtocols = []
--   , dataAnnotations = []
--   , dataRawValue = Nothing
--   , typeAlias = False
--   , newtypeTag = False
--   , lowerFirstField = True
--   , lowerFirstCase = True
--   , omitFields = const Keep
--   , omitCases = const Keep
--   , makeBase = (False, Nothing, [])
--   }
-- @
--
defaultOptions :: Options
defaultOptions = Options
  { typeConstructorModifier = id
  , fieldLabelModifier = id
  , constructorModifier = id
  , optionalExpand = False
  , generateToMoatType = True
  , generateToMoatData = True
  , dataInterfaces = []
  , dataProtocols = []
  , dataAnnotations = []
  , dataRawValue = Nothing
  , typeAlias = False
  , newtypeTag = False
  , lowerFirstField = True
  , lowerFirstCase = True
  , omitFields = const Keep
  , omitCases = const Keep
  , makeBase = (False, Nothing, [])
  }

data KeepOrDiscard = Keep | Discard
  deriving stock (Eq, Ord, Show, Read)
