{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}

module Moat.Types
  ( MoatType (..),
    MoatData (..),
    Backend (..),
    Protocol (..),
    Interface (..),
    Options (..),
    KeepOrDiscard (..),
    Annotation (..),
    defaultOptions,
  )
where

import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)

-- | An AST representing a type.
data MoatType
  = -- | Unit (called "Unit/Void" in swift). Empty struct type.
    Unit
  | -- | Bool
    Bool
  | -- | Character
    Character
  | -- | String. Named 'Str' to avoid conflicts with
    --   'Data.Aeson.String'.
    Str
  | -- | signed machine integer
    I
  | -- | signed 8-bit integer
    I8
  | -- | signed 16-bit integer
    I16
  | -- | signed 32-bit integer
    I32
  | -- | signed 64-bit integer
    I64
  | -- | unsigned machine integer
    U
  | -- | unsigned 8-bit integer
    U8
  | -- | unsigned 16-bit integer
    U16
  | -- | unsigned 32-bit integer
    U32
  | -- | unsigned 64-bit integer
    U64
  | -- | 32-bit floating point
    F32
  | -- | 64-bit floating point
    F64
  | -- | Increased-precision floating point
    Decimal
  | -- | 64-bit big integer
    BigInt
  | -- | 2-tuple
    Tuple2 MoatType MoatType
  | -- | 3-tuple
    Tuple3 MoatType MoatType MoatType
  | -- | Maybe type
    Optional MoatType
  | -- | Either type
    --
    --   /Note/: The error type in Swift must
    --   implement the @Error@ protocol. This library
    --   currently does not enforce this.
    Result MoatType MoatType
  | -- | Set type
    Set MoatType
  | -- | Dictionary type
    Dictionary MoatType MoatType
  | -- | array type
    Array MoatType
  | -- | function type
    App MoatType MoatType
  | -- | polymorphic type variable
    Poly String
  | -- | a concrete type variable, and its
    --   type variables. Will typically be generated
    --   by 'getShwifty'.
    Concrete
      { -- | the name of the type
        concreteName :: String,
        -- | the type's type variables
        concreteTyVars :: [MoatType]
      }
  | -- | A @Tagged@ typealias, for newtyping
    --   in a way that doesn't break Codable.
    --
    --   See 'getShwiftyWithTags' for examples.
    Tag
      { -- | the name of the type
        tagName :: String,
        -- | the type constructor of the type
        --   to which this alias belongs
        tagParent :: String,
        -- | the type that this represents
        tagTyp :: MoatType,
        -- | does the type need disambiguation?
        --
        --   This will happen if there are multiple
        --   tags with the same type. This is needed
        --   to maintain safety.
        tagDisambiguate :: Bool
      }
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
  = -- | A struct (product type)
    MoatStruct
      { -- | The name of the struct
        structName :: String,
        -- | The struct's type variables
        structTyVars :: [String],
        -- | The interfaces which the struct implements
        structInterfaces :: [Interface],
        -- | The protocols which the struct implements
        structProtocols :: [Protocol],
        -- | The annotations on the struct
        structAnnotations :: [Annotation],
        -- | The fields of the struct. the pair
        --   is interpreted as (name, type).
        structFields :: [(String, MoatType)],
        -- | Private types of the struct. Typically
        --   populated by setting 'makeBase'.
        --
        --   Only used by the Swift backend.
        structPrivateTypes :: [MoatData],
        -- | The tags of the struct. See 'Tag'.
        --
        --   Only used by the Swift backend.
        structTags :: [MoatType]
      }
  | -- | An enum (sum type)
    MoatEnum
      { -- | The name of the enum
        enumName :: String,
        -- | The enum's type variables
        enumTyVars :: [String],
        -- | The interfaces (Kotlin) which the enum implements
        enumInterfaces :: [Interface],
        -- | The interfaces (Kotlin) which the enum implements
        enumProtocols :: [Protocol],
        -- | The annotations on the enum
        enumAnnotations :: [Annotation],
        -- | The cases of the enum. the type
        --   can be interpreted as
        --   (name, [(label, type)]).
        enumCases :: [(String, [(Maybe String, MoatType)])],
        -- | The rawValue of an enum. See
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
        enumRawValue :: Maybe MoatType,
        -- | Private types of the enum. Typically
        --   populated by setting 'makeBase'.
        --
        --   Only used by the Swift backend.
        enumPrivateTypes :: [MoatData],
        -- | The tags of the struct. See 'Tag'.
        --
        --   Only used by the Swift backend.
        enumTags :: [MoatType]
      }
  | -- | A newtype.
    --   Kotlin backend: becomes an inline class.
    --   Swift backend: Becomes an empty enum with a tag.
    MoatNewtype
      { newtypeName :: String,
        newtypeTyVars :: [String],
        newtypeField :: (String, MoatType),
        newtypeInterfaces :: [Interface],
        newtypeProtocols :: [Protocol], -- TODO: remove this?
        newtypeAnnotations :: [Annotation]
      }
  | -- | A /top-level/ type alias
    MoatAlias
      { -- | the name of the type alias
        aliasName :: String,
        -- | the type variables of the type alias
        aliasTyVars :: [String],
        -- | the type this represents (RHS)
        aliasTyp :: MoatType
      }
  deriving stock (Eq, Read, Show, Generic)

data Backend
  = Kotlin
  | Swift
  deriving stock (Eq, Read, Show)

data Interface
  = Parcelable
  | -- | Derive from some Kotlin interface
    -- @
    --     data class A(
    --       ...
    --     ) : {String}
    -- @
    RawInterface String
  | -- | Derive from a sealed class
    -- @
    --     data class A(
    --       ...
    --     ) : {String}()
    -- @
    LinkEnumInterface String
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
  = -- | The 'Hashable' protocol.
    --   See https://developer.apple.com/documentation/swift/hashable
    Hashable
  | -- | The 'Codable' protocol.
    --   See https://developer.apple.com/documentation/swift/codable
    Codable
  | -- | The 'Equatable' protocol.
    --   See https://developer.apple.com/documentation/swift/equatable
    Equatable
  | -- | A user-specified protocol.
    OtherProtocol String
  deriving stock (Eq, Read, Show, Generic)
  deriving stock (Lift)

-- | Options that specify how to
--   encode your 'MoatData' to a concrete type.
--
--   Options can be set using record syntax on
--   'defaultOptions' with the fields below.
data Options = Options
  { -- | Function applied to type constructor names.
    --   The default ('id') makes no changes.
    typeConstructorModifier :: String -> String,
    -- | Function applied to field labels.
    --   Handy for removing common record prefixes,
    --   for example. The default ('id') makes no
    --   changes.
    fieldLabelModifier :: String -> String,
    -- | Function applied to value constructor names.
    --   The default ('id') makes no changes.
    constructorModifier :: String -> String,
    -- | Whether or not to truncate Optional types.
    --   Normally, an Optional ('Maybe') is encoded
    --   as "A?", which is syntactic sugar for
    --   "Optional\<A\>". The default value ('False')
    --   will keep it as sugar. A value of 'True'
    --   will expand it to be desugared.
    optionalExpand :: Bool,
    -- | Whether or not to generate a 'ToMoatType'
    --   instance. Sometimes this can be desirable
    --   if you want to define the instance by hand,
    --   or the instance exists elsewhere.
    --   The default is 'True', i.e., to generate
    --   the instance.
    generateToMoatType :: Bool,
    -- | Whether or not to generate a 'ToSwiftData'
    --   instance. Sometime this can be desirable
    --   if you want to define the instance by hand,
    --   or the instance exists elsewhere.
    --   The default is 'True', i.e., to generate
    --   the instance.
    generateToMoatData :: Bool,
    -- | Interfaces to add to a type.
    --   The default (@[]@) will add none.
    --
    --   This is only meaningful on the Kotlin
    --   backend.
    dataInterfaces :: [Interface],
    -- | Protocols to add to a type.
    --   The default (@[]@) will add none.
    --
    --   This is only meaningful on the Swift
    --   backend.
    dataProtocols :: [Protocol],
    -- | Annotations to add to a type.
    --   The default (@[]@) will add none.
    --
    --   This is only meaningful on the Kotlin
    --   backend.
    dataAnnotations :: [Annotation],
    -- | The rawValue of an enum. See
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
    dataRawValue :: Maybe MoatType,
    -- | Whether or not to generate a newtype as
    --   a type alias. Consider if you want this
    --   or to use 'getShwiftyWithTags' instead.
    --
    --   The default ('False') will generate newtypes
    --   as their own structs.
    typeAlias :: Bool,
    -- | Whether or not to generate a newtype as an
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
    newtypeTag :: Bool,
    -- | Whether or not to lower-case the first
    --   character of a field after applying all
    --   modifiers to it.
    --
    --   The default ('True') will do so.
    lowerFirstField :: Bool,
    -- | Whether or not to lower-case the first
    --   character of a case after applying all
    --   modifiers to it.
    --
    --   The default ('True') will do so.
    lowerFirstCase :: Bool,
    -- | Fields to omit from a struct when
    --   generating types.
    --
    --   The default (@[]@) will omit nothing.
    omitFields :: String -> KeepOrDiscard,
    -- | Cases to omit from an enum when
    --   generating types.
    --
    --   The default (@[]@) will omit nothing.
    omitCases :: String -> KeepOrDiscard,
    -- | Whether or not to make a base type,
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
    makeBase :: (Bool, Maybe MoatType, [Protocol])
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
defaultOptions :: Options
defaultOptions =
  Options
    { typeConstructorModifier = id,
      fieldLabelModifier = id,
      constructorModifier = id,
      optionalExpand = False,
      generateToMoatType = True,
      generateToMoatData = True,
      dataInterfaces = [],
      dataProtocols = [],
      dataAnnotations = [],
      dataRawValue = Nothing,
      typeAlias = False,
      newtypeTag = False,
      lowerFirstField = True,
      lowerFirstCase = True,
      omitFields = const Keep,
      omitCases = const Keep,
      makeBase = (False, Nothing, [])
    }

data KeepOrDiscard = Keep | Discard
  deriving stock (Eq, Ord, Show, Read)
