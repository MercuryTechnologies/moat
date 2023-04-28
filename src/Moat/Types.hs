{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}

module Moat.Types
  ( Annotation (..),
    Backend (..),
    EncodingStyle (..),
    EnumCase (..),
    EnumEncodingStyle (..),
    Field (..),
    Interface (..),
    KeepOrDiscard (..),
    MoatData (..),
    MoatType (..),
    Options (..),
    Protocol (..),
    SumOfProductEncodingOptions (..),
    defaultOptions,
    defaultSumOfProductEncodingOptions,
  )
where

import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)

-- | An AST representing a type
data MoatType
  = -- | Equivalent to '()' in Haskell, "Void" in Swift
    Unit
  | -- | The boolean type
    Bool
  | -- | A single character
    Character
  | -- | String. Named 'Str' to avoid conflicts with 'Data.Aeson.String'.
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
    --   by 'getMoat'.
    Concrete
      { concreteName :: String
      -- ^ the name of the type
      , concreteTyVars :: [MoatType]
      -- ^ the type's type variables
      }
  | -- | A @Tagged@ typealias, for newtyping
    --   in a way that doesn't break Codable.
    --
    --   See 'getMoatWithTags' for examples.
    Tag
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
  deriving stock (Eq, Show, Read)
  deriving stock (Generic)
  deriving stock (Lift)

-- | A datatype, either a struct (product type), enum (sum type), or type
-- alias. Haskell types are sums-of-products, so the way we differentiate when
-- doing codegen, is that types with a single constructor will be converted to
-- a struct, and those with two or more constructors will be converted to an
-- enum. Types with 0 constructors will be converted to an empty enum.
data MoatData
  = -- | A record, struct, or product type
    MoatStruct
      { structName :: String
      -- ^ The name of the struct
      , structDoc :: Maybe String
      -- ^ Haddock documentation for the struct.
      , structTyVars :: [String]
      -- ^ The struct's type variables
      , structInterfaces :: [Interface]
      -- ^ The kotlin interfaces which the struct implements
      , structProtocols :: [Protocol]
      -- ^ The swift protocols which the struct implements
      , structAnnotations :: [Annotation]
      -- ^ The kotlin annotations on the struct
      , structFields :: [Field]
      -- ^ The fields of the struct.
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
  | -- | An enum, sum, or coproduct type
    MoatEnum
      { enumName :: String
      -- ^ The name of the enum
      , enumDoc :: Maybe String
      -- ^ Haddock documentation for the enum.
      , enumTyVars :: [String]
      -- ^ The enum's type variables
      , enumInterfaces :: [Interface]
      -- ^ The interfaces (Kotlin) which the enum implements
      , enumProtocols :: [Protocol]
      -- ^ The protocols (Swift) which the enum implements
      , enumAnnotations :: [Annotation]
      -- ^ The annotations (Kotlin) on the enum
      , enumCases :: [EnumCase]
      -- ^ The cases of the enum.
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
      , enumSumOfProductEncodingOption :: SumOfProductEncodingOptions
      , enumEnumEncodingStyle :: EnumEncodingStyle
      }
  | -- | A newtype.
    --   Kotlin backend: becomes a value class.
    --   Swift backend: Becomes an empty enum with a tag.
    MoatNewtype
      { newtypeName :: String
      , newtypeDoc :: Maybe String
      -- ^ Haddock documentation for the newtype.
      , newtypeTyVars :: [String]
      , newtypeField :: Field
      , newtypeInterfaces :: [Interface]
      , newtypeProtocols :: [Protocol] -- TODO: remove this?
      , newtypeAnnotations :: [Annotation]
      }
  | -- | A /top-level/ type alias
    MoatAlias
      { aliasName :: String
      -- ^ the name of the type alias
      , aliasDoc :: Maybe String
      -- ^ Haddock documentation for the alias.
      , aliasTyVars :: [String]
      -- ^ the type variables of the type alias
      , aliasTyp :: MoatType
      -- ^ the type this represents (RHS)
      }
  deriving stock (Eq, Read, Show, Generic)

-- | An enum case.
data EnumCase = EnumCase
  { enumCaseName :: String
  , enumCaseDoc :: Maybe String
  , enumCaseFields :: [Field]
  }
  deriving stock (Eq, Read, Show)

-- | A named field for enum cases and structs.
data Field = Field
  { fieldName :: String
  , fieldType :: MoatType
  , fieldDoc :: Maybe String
  }
  deriving stock (Eq, Read, Show)

data Backend
  = Kotlin
  | Swift
  deriving stock (Eq, Read, Show)

data Interface
  = -- | Use the Parcelable interface, may want the 'Parcelize' annotation as well
    --
    -- >   data class A(
    -- >     ...
    -- >   ) : Parcelable
    Parcelable
  | -- | /escape hatch/ Use an arbitrary interface
    --
    -- >   data class A(
    -- >     ...
    -- >   ) : {String}
    RawInterface String
  | -- | /escape hatch/ Use an arbitrary linked interface
    --
    -- >   data class A(
    -- >     ...
    -- >   ) : {String}()
    LinkEnumInterface String
  deriving stock (Eq, Read, Show)
  deriving stock (Lift)

data Annotation
  = -- | The 'JvmInline' annotation, see https://kotlinlang.org/docs/inline-classes.html
    JvmInline
  | -- | The 'Parcelize' annotation, see https://developer.android.com/kotlin/parcelize
    -- automatically generates a Parcelable implementation for the type
    Parcelize
  | -- | The 'Serializable' annotation, see https://developer.android.com/reference/kotlin/java/io/Serializable
    Serializable
  | -- | /escape hatch/ to add an arbitrary annotation
    RawAnnotation String
  | -- | The 'SerialName' annotation is an annotation for products in a sum of
    -- products and only applies when used on the sum, see
    -- https://kotlin.github.io/kotlinx.serialization/kotlinx-serialization-core/kotlinx-serialization-core/kotlinx.serialization/-serial-name/index.html
    SerialName
  deriving stock (Eq, Read, Show)
  deriving stock (Lift)

-- | A 'Protocol' are annotations added when writing Swift types via 'prettySwiftType'
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

-- | Options that specify how to encode your 'MoatData' to a concrete type.
--
--   Options can be set using record update syntax on 'defaultOptions' with the
--   fields below.
data Options = Options
  { -- | A function to apply to type constructor names.
    --   The default makes no changes
    typeConstructorModifier :: String -> String,
    -- | A function to apply to field labels.  Handy for removing common record
    -- prefixes, for example. The default makes no changes
    fieldLabelModifier :: String -> String,
    -- | Whether or not to lowercase the first letters of fields. Applied before
    -- 'fieldLabelModifier'.
    fieldLabelLowerFirst :: Bool,
    -- | A function to apply to data constructor names. The default makes no
    -- changes.
    constructorModifier :: String -> String,
    -- | Whether or not to lowercase the first letters of constructors. Applied
    -- after 'constructorModifier'.
    constructorLowerFirst :: Bool,
    -- | Whether or not to generate a 'ToMoatType' instance. Sometimes this can
    -- be desirable if you want to define the instance by hand, or the instance
    -- exists elsewhere.  The default is 'True', i.e., to generate the
    -- instance.
    generateToMoatType :: Bool,
    -- | Whether or not to generate a 'ToMoatData' instance. Sometimes this can
    -- be desirable if you want to define the instance by hand, or the instance
    -- exists elsewhere.  The default is 'True', i.e., to generate the
    -- instance.
    generateToMoatData :: Bool,
    -- | Whether or not to generate documentation comments. This works by
    -- translating Haddock documentation annotations to the target language's
    -- documentation comment syntax. The default  is 'True', i.e. to generate
    -- documentation comments.
    generateDocComments :: Bool,
    -- | Kotlin interfaces to add to a type.
    --   The default (@[]@) will add none.
    dataInterfaces :: [Interface],
    -- | Swift protocols to add to a type.
    --   The default (@[]@) will add none.
    dataProtocols :: [Protocol],
    -- | Kotlin annotations to add to a type.
    --   The default (@[]@) will add none.
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
    --   or to use 'getMoatWithTags' instead.
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
    -- > $(getMoatWith (defaultOptions { newtypeTag = True }) ''NonEmpyText)
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
    -- | A function to apply to fields and choose whether, or not, to keep them
    --
    -- e.g.
    --
    -- > \case
    -- >   "discardThisField" -> Discard
    -- >   "keepThisField" -> Keep
    --
    --   The default (@const Keep@) will not discard fields
    omitFields :: String -> KeepOrDiscard,
    -- | A function to apply to enum cases and choose whether, or not, to keep them
    --
    --   The default (@const Keep@) will omit nothing.
    omitCases :: String -> KeepOrDiscard,
    -- | These fields are relied upon and must exist in the record.
    --
    -- The default @[]@ will not require any fields to exist.
    --
    -- This can be used with @omitFields = const Discard@ to ensure fields are
    -- retained for client compatibility.
    strictFields :: [String],
    -- | These enum cases are relied upon and must exist in the sum type.
    --
    -- The default @[]@ will not require any cases to exist.
    --
    -- This is likely less useful than 'strictFields' but may be useful if you
    -- only send certain enum cases to clients.
    strictCases :: [String],
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
    makeBase :: (Bool, Maybe MoatType, [Protocol]),
    -- TODO: This could use some better documentation after implementing this functionality
    optionalExpand :: Bool
  -- ^ Whether or not to truncate Optional types.
  --   Normally, an Optional ('Maybe') is encoded
  --   as "A?", which is syntactic sugar for
  --   "Optional\<A\>". The default value ('False')
  --   will keep it as sugar. A value of 'True'
  --   will expand it to be desugared.
  , sumOfProductEncodingOptions :: SumOfProductEncodingOptions
  -- ^ Only applies for a sum in a sum of products. The options
  -- determine the rendering style for the sum of products.
  -- The user is responsible for choosing the right options
  -- for the products in a SOP. See 'SumOfProductEncodingOptions'
  , enumEncodingStyle :: EnumEncodingStyle
  -- ^ Enum encoding style.
  --
  --   Only applies for a "C-style" sum-of-products type, where
  --   each sum constructor has zero type arguments. See
  --   'EnumEncodingStyle'.
  --
  --   This option is only meaningful on the Kotlin backend.
  }

data SumOfProductEncodingOptions = SumOfProductEncodingOptions
  { encodingStyle :: EncodingStyle
  -- ^ The encoding style for the sum of product, the library matches the options
  -- available in aeson, see
  -- https://hackage.haskell.org/package/aeson/docs/Data-Aeson-TH.html#t:SumEncoding
  -- and 'EncodingStyle'
  , sumAnnotations :: [Annotation]
  -- ^ The annotations to add solely to sum in the sum of product, e.g.
  -- in kotlinx.serialization we want to add '@JsonClassDiscriminator("tag")'
  -- annotation to the sum type but not the products!
  , contentsFieldName :: String
  -- ^ The field name to use for the products, aeson uses "contents" for the TaggedObject
  -- style. This is unused in the 'TaggedFlatObjectStyle'
  }
  deriving stock (Eq, Read, Show, Lift)

-- | The resulting enum style for our datatype. This names match
-- the style in Aeson. A 'TaggedObjectStyle' will have a JSON
-- payload like,
--
-- @
-- {
--   "tag": ...,
--   "contents": ...
-- }
-- @
--
-- In 'TaggedFlatObjectStyle', the contents are unpacked at the same
-- level as "tag"
data EncodingStyle = TaggedObjectStyle | TaggedFlatObjectStyle
  deriving stock (Eq, Read, Show, Lift)

-- | The default 'SumOfProductEncodingOptions'
defaultSumOfProductEncodingOptions :: SumOfProductEncodingOptions
defaultSumOfProductEncodingOptions =
  SumOfProductEncodingOptions
    { encodingStyle = TaggedFlatObjectStyle
    , sumAnnotations = []
    , contentsFieldName = "contents"
    }

-- | Enum encoding style.
--
-- 'EnumClassStyle' will emit a normal enum class.
--
-- 'ValueClassStyle' will emit an inline type wrapping a string value,
-- with static members corresponding to enum cases. This can be useful
-- for maintaining backward compatibility when adding enum cases.
data EnumEncodingStyle = EnumClassStyle | ValueClassStyle
  deriving stock (Eq, Read, Show, Lift)

-- | The default 'Options'.
--
-- @
-- defaultOptions :: Options
-- defaultOptions = Options
--   { typeConstructorModifier = id
--   , fieldLabelModifier = id
--   , fieldLabelLowerFirst = True,
--   , constructorModifier = id
--   , constructorLowerFirst = True,
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
--   , strictFields = []
--   , strictCases = []
--   , makeBase = (False, Nothing, [])
--   , optionalExpand = False
--   , sumOfProductEncodingOptions = defaultSumOfProductEncodingOptions
--   , enumEncodingStyle = EnumClassStyle
--   }
-- @
defaultOptions :: Options
defaultOptions =
  Options
    { typeConstructorModifier = id,
      fieldLabelModifier = id,
      fieldLabelLowerFirst = True,
      constructorModifier = id,
      constructorLowerFirst = True,
      generateToMoatType = True,
      generateToMoatData = True,
      generateDocComments = True,
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
      strictFields = [],
      strictCases = [],
      makeBase = (False, Nothing, []),
      optionalExpand = False,
      sumOfProductEncodingOptions = defaultSumOfProductEncodingOptions,
      enumEncodingStyle = EnumClassStyle
    }

data KeepOrDiscard = Keep | Discard
  deriving stock (Eq, Ord, Show, Read)
