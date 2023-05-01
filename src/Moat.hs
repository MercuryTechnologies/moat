{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | The moat library allows generation of Swift and Kotlin types (structs
-- and enums) from Haskell ADTs, using Template Haskell. The main entry point
-- to the library should be the documentation and examples of 'mobileGen'. See
-- also 'mobileGenWith' and 'mobileGenWithTags'.
--
-- This library is in alpha and there are a number of known bugs which
-- shouldn't affect most users. See the issue tracker to see what those are.
--
-- There are probably many bugs/some weird behavior when it comes to data
-- families. Please report any issues on the issue tracker.
module Moat
  ( -- * Type classes used to facilitate conversions
    ToMoatType (..),
    ToMoatData (..),

    -- * Template Haskell functions to generate conversion type classes
    mobileGen,
    mobileGenWith,
    mobileGenWithTags,

    -- * Intermediate representation for conversions
    MoatType (..),
    MoatData (..),
    Protocol (..),
    Interface (..),
    Annotation (..),
    Field (..),
    EnumCase (..),

    -- * Options for encoding types

    -- ** Option type and defaults
    Options,
    defaultOptions,
    EncodingStyle (..),
    SumOfProductEncodingOptions (..),
    defaultSumOfProductEncodingOptions,
    EnumEncodingStyle (..),

    -- ** Helper type for omissions
    KeepOrDiscard (..),

    -- ** Available options
    fieldLabelModifier,
    fieldLabelLowerFirst,
    constructorModifier,
    constructorLowerFirst,
    generateToMoatType,
    generateToMoatData,
    generateDocComments,
    dataProtocols,
    dataInterfaces,
    dataAnnotations,
    dataRawValue,
    typeAlias,
    newtypeTag,
    lowerFirstCase,
    lowerFirstField,
    omitFields,
    omitCases,
    strictFields,
    strictCases,
    makeBase,
    sumOfProductEncodingOptions,
    enumEncodingStyle,

    -- * Pretty-printing

    -- ** Functions
    prettyKotlinData,
    prettySwiftData,

    -- * Utility
    aliasToNewtype,
    newtypeToAlias,
    X,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Data.Bool (bool)
import qualified Data.Char as Char
import Data.Foldable (foldl', foldlM, foldr')
import Data.Functor ((<&>))
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Text as TS
import Data.Void (Void)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Language.Haskell.TH hiding (TyVarBndr (..), newName, stringE, tupE)
import qualified Language.Haskell.TH
import Language.Haskell.TH.Datatype
import qualified Language.Haskell.TH.Syntax as Syntax
import Language.Haskell.TH.Syntax.Compat
import Moat.Class
import Moat.Pretty.Kotlin (prettyKotlinData)
import Moat.Pretty.Swift (prettySwiftData)
import Moat.Types hiding (newtypeName)
import qualified Moat.Types
import Prelude hiding (Enum (..))

#if MIN_VERSION_template_haskell(2,17,0)
type TyVarBndr = Language.Haskell.TH.TyVarBndr ()

{-# COMPLETE PlainTV, KindedTV #-}
pattern PlainTV :: Name -> Syntax.TyVarBndr ()
pattern PlainTV n = Language.Haskell.TH.PlainTV n ()

pattern KindedTV :: Name -> Kind -> Syntax.TyVarBndr ()
pattern KindedTV n k <- Language.Haskell.TH.KindedTV n _ k

#else
type TyVarBndr = Language.Haskell.TH.TyVarBndr

{-# COMPLETE PlainTV, KindedTV #-}
pattern PlainTV :: Name -> Syntax.TyVarBndr
pattern PlainTV n = Language.Haskell.TH.PlainTV n
pattern KindedTV :: Name -> Kind -> Syntax.TyVarBndr
pattern KindedTV n k = Language.Haskell.TH.KindedTV n k
#endif

-- Used internally to reflect polymorphic type
-- variables into TH, then reify them into 'Poly'.
--
-- See the Rose tree section below
data SingSymbol (x :: Symbol)

instance KnownSymbol x => ToMoatType (SingSymbol x) where
  toMoatType _ = Poly (symbolVal (Proxy @x))

-- TODO: Add tests which flex the need for this type, see src/Test.hs.bak for examples

-- | A filler type to be used when pretty-printing.
--   The codegen used by moat doesn't look at
--   at what a type's type variables are instantiated
--   to, but rather at the type's top-level
--   definition. However,
--   to make GHC happy, you will have to fill in type
--   variables with unused types. To get around this,
--   you could also use something like
--   `-XQuantifiedConstraints`, or existential types,
--   but we leave that to the user to handle.
type X = Void

ensureEnabled :: Extension -> MoatM ()
ensureEnabled ext = do
  enabled <- lift $ isExtEnabled ext
  if enabled
    then pure ()
    else throwError $ ExtensionNotEnabled ext

-- | Generate 'ToMoatData' and 'ToMoatType' instances for your type.
-- 'ToMoatType' instances are typically used to build cases or fields, whereas
-- 'ToMoatData' instances are for building structs and enums. See the examples
-- below for using 'mobileGen'. There are also generated code snippets for
-- Swift which are generated by 'prettySwiftData'
--
-- === __Examples__
--
-- > -- A simple sum type
-- > data SumType = Sum1 | Sum2 | Sum3
-- > mobileGen ''SumType
--
-- > -- A simple product type
-- > data ProductType = ProductType { x :: Int, y :: Int }
-- > mobileGen ''ProductType
mobileGen :: Name -> Q [Dec]
mobileGen = mobileGenWith defaultOptions

-- | Like 'mobileGen', but lets you supply
--   your own 'Options'. Click the examples
--   for some clarification of what you can do.
--
-- === __Examples__
--
-- Drop Haskell's requirement on unique field names when generating your types
--
-- > data PrefixedFields = MkPrefixedFields { prefixedFieldsX :: Int, prefixedFieldsY :: Int }
-- > $(mobileGenWith (defaultOptions { fieldLabelModifier = drop (length "prefixedFields") }) ''PrefixedFields)
--
-- Add Swift protocols for your datatype
--
-- > data PrefixedCons = MkPrefixedConsLeft | MkPrefixedConsRight
-- > $(mobileGenWith (defaultOptions { constructorModifier = drop (length "MkPrefixedCons"), dataProtocols = [Codable] }) ''PrefixedCons)
mobileGenWith :: Options -> Name -> Q [Dec]
mobileGenWith o = mobileGenWithTags o []

data NewtypeInfo = NewtypeInfo
  { newtypeName :: Name
  -- ^ The type constructors name
  , newtypeVars :: [TyVarBndr]
  -- ^ Type constructor parameters, see 'TyVarBndr'
  , newtypeInstTypes :: [Type]
  -- ^ Type constructor instance types, see 'Type'
  , newtypeVariant :: DatatypeVariant
  -- ^ Whether or not the type is a newtype or newtype instance
  , newtypeCon :: ConstructorInfo
  -- ^ The data constructor information
  }

-- | Reify a newtype.
reifyNewtype :: Name -> MoatM NewtypeInfo
reifyNewtype n = do
  DatatypeInfo {..} <- lift $ reifyDatatype n
  case (datatypeCons, datatypeVariant) of
    ([c], Newtype) -> do
      pure
        NewtypeInfo
          { newtypeName = datatypeName
          , newtypeVars = datatypeVars
          , newtypeInstTypes = datatypeInstTypes
          , newtypeVariant = datatypeVariant
          , newtypeCon = c
          }
    ([c], NewtypeInstance) -> do
      pure
        NewtypeInfo
          { newtypeName = datatypeName
          , newtypeVars = datatypeVars
          , newtypeInstTypes = datatypeInstTypes
          , newtypeVariant = datatypeVariant
          , newtypeCon = c
          }
    _ -> do
      throwError $ NotANewtype n

-- Generate the tags for a type.
-- Also generate the ToMoatType instance for each tag
-- type. We can't just expect people to do this
-- with a separate 'mobileGen' call, because
-- they will generate the wrong code, since other
-- types with a tag that isn't theirs won't generate
-- well-scoped fields.
getTags ::
  () =>
  -- | name of parent type
  Name ->
  -- | tags
  [Name] ->
  MoatM ([Exp], [Dec])
getTags parentName ts = do
  let b = length ts > 1
  disambiguate <- lift $ examineSplice [||b||]
  foldlM
    ( \(es, ds) n -> do
        NewtypeInfo {..} <- reifyNewtype n
        let ConstructorInfo {..} = newtypeCon

        -- generate the tag
        let tyconName = case newtypeVariant of
              NewtypeInstance -> constructorName
              _ -> newtypeName
        typ <- case constructorFields of
          [ty] -> pure ty
          _ -> throwError $ NotANewtype newtypeName
        let tag =
              RecConE
                'Tag
                [ ('tagName, unqualName tyconName)
                , ('tagParent, unqualName parentName)
                , ('tagTyp, toMoatTypeEPoly typ)
                , ('tagDisambiguate, unType disambiguate)
                ]

        -- generate the instance
        (context, instHeadTy) <-
          buildTypeInstance newtypeName ClassType newtypeInstTypes newtypeVariant
        -- we do not want to strip here
        clauseTy <- tagToMoatType tyconName typ parentName
        swiftTyInst <-
          lift $
            instanceD
              (pure context)
              (pure instHeadTy)
              [ funD
                  'toMoatType
                  [ clause [] (normalB (pure clauseTy)) []
                  ]
              ]

        pure (es ++ [tag], ds ++ [swiftTyInst])
    )
    ([], [])
    ts

getToMoatType ::
  () =>
  -- | options
  Options ->
  -- | type name
  Name ->
  -- | type variables
  [Type] ->
  -- | type variant
  DatatypeVariant ->
  -- | constructors
  [ConstructorInfo] ->
  MoatM [Dec]
getToMoatType Options {..} parentName instTys variant cons =
  if generateToMoatType
    then do
      (context, instHead) <- buildTypeInstance parentName ClassType instTys variant
      clauseTy <- case variant of
        NewtypeInstance -> case cons of
          [ConstructorInfo {..}] -> do
            newtypToMoatType constructorName instTys
          _ -> do
            throwError ExpectedNewtypeInstance
        _ -> do
          typToMoatType newtypeTag parentName instTys
      inst <-
        lift $
          instanceD
            (pure context)
            (pure instHead)
            [ funD
                'toMoatType
                [ clause [] (normalB (pure clauseTy)) []
                ]
            ]
      pure [inst]
    else do
      pure []

getToMoatData ::
  () =>
  -- | options
  Options ->
  -- | type name
  Name ->
  -- | haddock
  Maybe String ->
  -- | type variables
  [Type] ->
  -- | type variant
  DatatypeVariant ->
  -- | tags
  [Exp] ->
  -- | constructors
  [ConstructorInfo] ->
  MoatM [Dec]
getToMoatData o@Options {..} parentName parentDoc instTys variant tags cons =
  if generateToMoatData
    then do
      (context, instHead) <- buildTypeInstance parentName ClassData instTys variant
      clauseData <- consToMoatType o parentName parentDoc instTys variant tags makeBase cons
      inst <-
        lift $
          instanceD
            (pure context)
            (pure instHead)
            [ funD
                'toMoatData
                [ clause [] (normalB (pure clauseData)) []
                ]
            ]
      pure [inst]
    else do
      pure []

-- | Like 'mobileGenWith', but lets you supply tags. Tags are type-safe
-- typealiases that are akin to newtypes in Haskell. The introduction of a
-- struct around something which is, say, a UUID in Swift means that the
-- default Codable instance will not work correctly. So we introduce a tag(s).
-- See the examples to see how this looks. Also, see
-- https://github.com/pointfreeco/swift-tagged, the library which these tags
-- use. The library is not included in any generated code.
--
-- === __Examples__
--
-- > -- Example of using the swift-tagged library:
-- > -- A type containing a database key
-- > data User = User { id :: UserId, name :: Text }
-- > -- the user key
-- > newtype UserId = UserId UUID
-- > $(mobileGenWithTags defaultOptions [ ''UserId ] ''User)
-- > -- A type that also contains the UserId
-- > data UserDetails = UserDetails { id :: UserId, lastName :: Text }
-- > mobileGen ''UserDetails
--
-- @
-- struct User {
--   let id: UserId
--   let name: String
--
--   typealias UserId = Tagged\<User,UUID\>
-- }
--
-- struct UserDetails {
--   let id: User.UserId
--   let lastName: String
-- }
-- @
--
-- > -- Example type with multiple tags
-- > newtype Name = MkName String
-- > newtype Email = MkEmail String
-- > data Person = Person { name :: Name, email :: Email }
-- > $(mobileGenWithTags defaultOptions [ ''Name, ''Email ] ''Person)
--
-- @
-- struct Person {
--     let name: Name
--     let email: Email
--
--     enum NameTag {}
--     typealias Name = Tagged\<NameTag, String\>
--
--     enum EmailTag {}
--     typealias Email = Tagged\<EmailTag, String\>
-- }
-- @
--
-- /Note/: Tags become newtypes on the Kotlin backend.
mobileGenWithTags ::
  () =>
  Options ->
  [Name] ->
  Name ->
  Q [Dec]
mobileGenWithTags o ts name = do
  r <- runExceptT $ do
    ensureEnabled ScopedTypeVariables
    ensureEnabled DataKinds
    DatatypeInfo
      { datatypeName = parentName
      , datatypeInstTypes = instTys
      , datatypeVariant = variant
      , datatypeCons = cons
      } <-
      lift $ reifyDatatype name
    noExistentials cons

    -- get tags/ToMoatType instances for tags
    (tags, extraDecs) <- getTags parentName ts

    -- get haddock for top-level declaration
    doc <- lift $ getDocWith o name

    dataInst <- getToMoatData o parentName doc instTys variant tags cons

    tyInst <- getToMoatType o parentName instTys variant cons
    pure $ dataInst ++ tyInst ++ extraDecs
  case r of
    Left e -> fail $ prettyMoatError e
    Right d -> pure d

noExistentials :: [ConstructorInfo] -> MoatM ()
noExistentials cs = forM_ cs $ \ConstructorInfo {..} ->
  case (constructorName, constructorVars) of
    (_, []) -> do
      pure ()
    (cn, cvs) -> do
      throwError $ ExistentialTypes cn cvs

data MoatError
  = SingleConNonRecord
      { _conName :: Name
      }
  | EncounteredInfixConstructor
      { _conName :: Name
      }
  | KindVariableCannotBeRealised
      { _typName :: Name
      , _kind :: Kind
      }
  | ExtensionNotEnabled
      { _ext :: Extension
      }
  | ExistentialTypes
      { _conName :: Name
      , _types :: [TyVarBndr]
      }
  | ExpectedNewtypeInstance
  | NotANewtype
      { _typName :: Name
      }
  | EncounteredNonTypeVariable
      { _funName :: String
      , _type :: Type
      }
  | ImproperNewtypeConstructorInfo
      { _conInfo :: ConstructorInfo
      }
  | MissingStrictFields
      { _missingFields :: [String]
      }
  | MissingStrictCases
      { _missingCases :: [String]
      }

prettyMoatError :: MoatError -> String
prettyMoatError = \case
  SingleConNonRecord (nameStr -> n) ->
    n
      ++ ": Cannot get moat with single-constructor "
      ++ "non-record types. This is due to a "
      ++ "restriction of Swift that prohibits structs "
      ++ "from not having named fields. Try turning "
      ++ n
      ++ " into a record!"
  EncounteredInfixConstructor (nameStr -> n) ->
    n
      ++ ": Cannot get moat with infix constructors. "
      ++ "Swift doesn't support them. Try changing "
      ++ n
      ++ " into a prefix constructor!"
  KindVariableCannotBeRealised (nameStr -> n) typ ->
    case prettyKindVar typ of
      Left err -> err
      Right (typStr, kindStr) ->
        n
          ++ ": Encountered a type variable ("
          ++ typStr
          ++ ") with a kind ("
          ++ kindStr
          ++ ") that can't "
          ++ "get moat! Moat needs to be able "
          ++ "to realise your kind variables to `*`, "
          ++ "since that's all that makes sense in "
          ++ "Swift. The only kinds that can happen with "
          ++ "are `*` and the free-est kind, `k`."
  ExtensionNotEnabled ext ->
    show ext
      ++ " is not enabled. Moat needs it to work!"
  -- TODO: make this not print out implicit kinds.
  -- e.g. for `data Ex = forall x. Ex x`, there are
  -- no implicit `TyVarBndr`s, but for
  -- `data Ex = forall x y z. Ex x`, there are two:
  -- the kinds inferred by `y` and `z` are both `k`.
  -- We print these out - this could be confusing to
  -- the end user. I'm not immediately certain how to
  -- be rid of them.
  ExistentialTypes (nameStr -> n) tys ->
    n
      ++ " has existential type variables ("
      ++ L.intercalate ", " (map prettyTyVarBndrStr tys)
      ++ ")! Moat doesn't support these."
  ExpectedNewtypeInstance ->
    "Expected a newtype instance. This is an "
      ++ "internal logic error. Please report it as a "
      ++ "bug."
  NotANewtype (nameStr -> n) ->
    n
      ++ " is not a newtype. This is an internal logic "
      ++ "error. Please report it as a bug."
  EncounteredNonTypeVariable funName typ ->
    "Encountered non-type variable in `"
      ++ funName
      ++ "`, expected VarT or SigT but got "
      ++ show typ
  ImproperNewtypeConstructorInfo conInfo ->
    "Expected `ConstructorInfo` with single field, but got "
      ++ show conInfo
  MissingStrictFields missingFields ->
    "Removing these fields will break clients: " ++ L.unwords missingFields
  MissingStrictCases missingCases ->
    "Removing these cases will break clients: " ++ L.unwords missingCases

prettyTyVarBndrStr :: TyVarBndr -> String
prettyTyVarBndrStr = \case
  PlainTV n -> go n
  KindedTV n _ -> go n
  where
    go = TS.unpack . head . TS.splitOn "_" . last . TS.splitOn "." . TS.pack . show

-- prettify the type and kind.
prettyKindVar :: Type -> Either String (String, String)
prettyKindVar = \case
  SigT typ k -> Right (go typ, go k)
  VarT n -> Right (nameStr n, "*")
  typ -> Left $ "Moat.prettyKindVar: used on a type without a kind signature. Type was: " ++ show typ
  where
    go = TS.unpack . head . TS.splitOn "_" . last . TS.splitOn "." . TS.pack . show . ppr

type MoatM = ExceptT MoatError Q

tagToMoatType ::
  () =>
  -- | name of the type constructor
  Name ->
  -- | type variables
  Type ->
  -- | parent name
  Name ->
  MoatM Exp
tagToMoatType tyconName typ parentName = do
  -- TODO: use '_' instead of matching
  value <- lift $ newName "value"
  ourMatch <-
    matchProxy $
      tagExp tyconName parentName typ False
  let matches = [pure ourMatch]
  lift $ lamE [varP value] (caseE (varE value) matches)

newtypToMoatType ::
  () =>
  -- | name of the constructor
  Name ->
  -- | type variables
  [Type] ->
  MoatM Exp
newtypToMoatType conName (stripConT -> instTys) = do
  typToMoatType False conName instTys

typToMoatType ::
  () =>
  -- | is this a newtype tag?
  Bool ->
  -- | name of the type
  Name ->
  -- | type variables
  [Type] ->
  MoatM Exp
typToMoatType newtypeTag parentName instTys = do
  let tyVars = map toMoatTypeECxt instTys
  let name =
        let parentStr = nameStr parentName
            accessedName =
              if newtypeTag
                then parentStr ++ "Tag." ++ parentStr
                else parentStr
         in stringE accessedName
  ourMatch <-
    matchProxy $
      RecConE
        'Concrete
        [ ('concreteName, name)
        , ('concreteTyVars, ListE tyVars)
        ]
  let matches = [pure ourMatch]
  lift $ lamCaseE matches

rawValueE :: Maybe MoatType -> Exp
rawValueE = \case
  Nothing -> ConE 'Nothing
  Just ty -> AppE (ConE 'Just) (ParensE (tyE ty))

-- god this is annoying. write a cleaner
-- version of this
tyE :: MoatType -> Exp
tyE = \case
  Unit -> ConE 'Unit
  Bool -> ConE 'Bool
  Character -> ConE 'Character
  Str -> ConE 'Str
  I -> ConE 'I
  I8 -> ConE 'I8
  I16 -> ConE 'I16
  I32 -> ConE 'I32
  I64 -> ConE 'I64
  U -> ConE 'U
  U8 -> ConE 'U8
  U16 -> ConE 'U16
  U32 -> ConE 'U32
  U64 -> ConE 'U64
  F32 -> ConE 'F32
  F64 -> ConE 'F64
  Decimal -> ConE 'Decimal
  BigInt -> ConE 'BigInt
  Poly s -> AppE (ConE 'Poly) (stringE s)
  Concrete tyCon tyVars -> AppE (AppE (ConE 'Concrete) (stringE tyCon)) (ListE (map tyE tyVars))
  Tuple2 e1 e2 -> AppE (AppE (ConE 'Tuple2) (tyE e1)) (tyE e2)
  Tuple3 e1 e2 e3 -> AppE (AppE (AppE (ConE 'Tuple3) (tyE e1)) (tyE e2)) (tyE e3)
  Optional e -> AppE (ConE 'Optional) (tyE e)
  Result e1 e2 -> AppE (AppE (ConE 'Result) (tyE e1)) (tyE e2)
  Set e -> AppE (ConE 'Set) (tyE e)
  Dictionary e1 e2 -> AppE (AppE (ConE 'Dictionary) (tyE e1)) (tyE e2)
  App e1 e2 -> AppE (AppE (ConE 'App) (tyE e1)) (tyE e2)
  Array e -> AppE (ConE 'Array) (tyE e)
  Tag {..} -> AppE (AppE (AppE (AppE (ConE 'Tag) (stringE tagName)) (stringE tagParent)) (tyE tagTyp)) (if tagDisambiguate then ConE 'True else ConE 'False)

consToMoatType ::
  () =>
  -- | options about how to encode things
  Options ->
  -- | name of type
  Name ->
  -- | haddock
  Maybe String ->
  -- | type variables
  [Type] ->
  -- | data type variant
  DatatypeVariant ->
  -- | tags
  [Exp] ->
  -- | Make base?
  (Bool, Maybe MoatType, [Protocol]) ->
  -- | constructors
  [ConstructorInfo] ->
  MoatM Exp
consToMoatType o@Options {..} parentName parentDoc instTys variant ts bs = \case
  [] -> do
    value <- lift $ newName "value"
    matches <- liftCons (mkVoid o parentName instTys ts)
    lift $ lamE [varP value] (caseE (varE value) matches)
  cons -> do
    -- TODO: use '_' instead of matching
    value <- lift $ newName "value"
    matches <- matchesWorker
    lift $ lamE [varP value] (caseE (varE value) matches)
    where
      -- bad name
      matchesWorker :: MoatM [Q Match]
      matchesWorker = case cons of
        [con] -> liftCons $ do
          case variant of
            NewtypeInstance -> do
              if typeAlias
                then mkNewtypeInstanceAlias parentDoc instTys con
                else mkNewtypeInstance o parentDoc instTys con
            Newtype -> do
              if
                  | newtypeTag -> do
                      mkTypeTag o parentName instTys con
                  | typeAlias -> do
                      mkTypeAlias parentName parentDoc instTys con
                  | otherwise -> do
                      mkNewtype o parentName parentDoc instTys con
            _ -> do
              mkProd o parentName parentDoc instTys ts con
        _ -> do
          -- 'strictCases' are required to exist and are always included.
          -- 'omitCases' will remove any remaining fields which are 'Discard'ed.
          let constructorNames = cons <&> \ConstructorInfo {..} -> nameStr constructorName
              missingConstructors = strictCases L.\\ constructorNames
          if null missingConstructors
            then do
              let cons' =
                    flip filter cons $
                      \ConstructorInfo {..} ->
                        let constructorStr = nameStr constructorName
                         in constructorStr `elem` strictCases
                              || omitCases (nameStr constructorName) == Keep
              cases <- forM cons' (mkCase o)
              ourMatch <-
                matchProxy
                  =<< lift (enumExp parentName parentDoc instTys dataInterfaces dataProtocols dataAnnotations cases dataRawValue ts bs sumOfProductEncodingOptions enumEncodingStyle)
              pure [pure ourMatch]
            else throwError $ MissingStrictCases missingConstructors

liftCons :: (Functor f, Applicative g) => f a -> f [g a]
liftCons x = (: []) . pure <$> x

-- Create the case (String, Maybe String, [(Maybe String, Ty, Maybe String)])
mkCaseHelper :: Options -> Name -> Maybe String -> [Exp] -> Exp
mkCaseHelper o name doc es =
  RecConE
    'EnumCase
    [ ('enumCaseName, caseName o name)
    , ('enumCaseDoc, prettyDoc doc)
    , ('enumCaseFields, ListE es)
    ]

mkCase ::
  () =>
  Options ->
  ConstructorInfo ->
  MoatM Exp
mkCase o = \case
  -- non-record
  ConstructorInfo
    { constructorVariant = NormalConstructor
    , constructorName = name
    , constructorFields = fields
    } ->
      do
        doc <- lift $ getDocWith o name
        pure $
          mkCaseHelper o name doc $
            fields
              <&> ( \typ ->
                      RecConE
                        'Field
                        [ ('fieldName, stringE "")
                        , ('fieldType, toMoatTypeEPoly typ)
                        , ('fieldDoc, ConE 'Nothing)
                        ]
                  )
  ConstructorInfo
    { constructorVariant = InfixConstructor
    , constructorName = name
    } -> throwError (EncounteredInfixConstructor name)
  -- records
  -- we turn names into labels
  ConstructorInfo
    { constructorVariant = RecordConstructor fieldNames
    , constructorName = name
    , constructorFields = fields
    } ->
      do
        doc <- lift $ getDocWith o name
        fieldDocs <- lift $ mapM (getDocWith o) fieldNames
        let cases = zipWith3 (caseField o) fieldNames fields fieldDocs
         in pure $ mkCaseHelper o name doc cases

caseField :: Options -> Name -> Type -> Maybe String -> Exp
caseField o n typ doc =
  RecConE
    'Field
    [ ('fieldName, mkLabel o n)
    , ('fieldType, toMoatTypeEPoly typ)
    , ('fieldDoc, prettyDoc doc)
    ]

onHeadWith :: Bool -> String -> String
onHeadWith toLower =
  if toLower
    then onHead Char.toLower
    else id

-- apply a function only to the head of a string
onHead :: (Char -> Char) -> String -> String
onHead f = \case [] -> []; (x : xs) -> f x : xs

mkLabel :: Options -> Name -> Exp
mkLabel Options {..} =
  stringE
    . fieldLabelModifier
    . bool id (onHeadWith lowerFirstField) fieldLabelLowerFirst
    . TS.unpack
    . last
    . TS.splitOn "."
    . TS.pack
    . show

mkNewtypeInstanceAlias ::
  () =>
  -- | haddock
  Maybe String ->
  -- | type variables
  [Type] ->
  -- | constructor info
  ConstructorInfo ->
  MoatM Match
mkNewtypeInstanceAlias doc (stripConT -> instTys) = \case
  ConstructorInfo
    { constructorName = conName
    , constructorFields = [field]
    } -> do
      lift $
        match
          (conP 'Proxy [])
          ( normalB
              ( pure
                  (aliasExp conName doc instTys field)
              )
          )
          []
  _ -> do
    throwError ExpectedNewtypeInstance

mkNewtypeInstance ::
  () =>
  -- | encoding options
  Options ->
  -- | haddock
  Maybe String ->
  -- | type variables
  [Type] ->
  -- | constructor info
  ConstructorInfo ->
  MoatM Match
mkNewtypeInstance o@Options {..} doc (stripConT -> instTys) = \case
  ConstructorInfo
    { constructorFields = [field]
    , ..
    } -> do
      matchProxy =<< lift (newtypeExp constructorName doc instTys dataInterfaces dataProtocols dataAnnotations (prettyField o (mkName "value") field Nothing))
  _ -> throwError ExpectedNewtypeInstance

-- make a newtype into an empty enum
-- with a tag
mkTypeTag ::
  () =>
  -- | options
  Options ->
  -- | type name
  Name ->
  -- | type variables
  [Type] ->
  -- | constructor info
  ConstructorInfo ->
  MoatM Match
mkTypeTag Options {..} typName instTys = \case
  ConstructorInfo
    { constructorFields = [field]
    } -> do
      let parentName =
            mkName
              (nameStr typName ++ "Tag")
      let tag = tagExp typName parentName field False
      matchProxy =<< lift (enumExp parentName Nothing instTys dataInterfaces dataProtocols dataAnnotations [] dataRawValue [tag] (False, Nothing, []) sumOfProductEncodingOptions enumEncodingStyle)
  _ -> throwError $ NotANewtype typName

-- make a newtype into a type alias
mkTypeAlias ::
  () =>
  -- | type name
  Name ->
  -- | haddock
  Maybe String ->
  -- | type variables
  [Type] ->
  -- | constructor info
  ConstructorInfo ->
  MoatM Match
mkTypeAlias typName doc instTys = \case
  ConstructorInfo
    { constructorFields = [field]
    } -> do
      lift $
        match
          (conP 'Proxy [])
          ( normalB
              (pure (aliasExp typName doc instTys field))
          )
          []
  _ -> throwError $ NotANewtype typName

-- | Make a void type (empty enum)
mkVoid ::
  () =>
  Options ->
  -- | type name
  Name ->
  -- | type variables
  [Type] ->
  -- | tags
  [Exp] ->
  MoatM Match
mkVoid Options {..} typName instTys ts =
  matchProxy
    =<< lift (enumExp typName Nothing instTys [] [] [] [] Nothing ts (False, Nothing, []) sumOfProductEncodingOptions enumEncodingStyle)

mkNewtype ::
  () =>
  Options ->
  Name ->
  Maybe String ->
  [Type] ->
  ConstructorInfo ->
  MoatM Match
mkNewtype o@Options {..} typName doc instTys = \case
  ConstructorInfo
    { constructorFields = [field]
    , constructorVariant = RecordConstructor [name]
    } -> do
      matchProxy =<< lift (newtypeExp typName doc instTys dataInterfaces dataProtocols dataAnnotations (prettyField o name field Nothing))
  ConstructorInfo
    { constructorFields = [field]
    } -> do
      matchProxy =<< lift (newtypeExp typName doc instTys dataInterfaces dataProtocols dataAnnotations (prettyField o (mkName "value") field Nothing))
  ci -> throwError $ ImproperNewtypeConstructorInfo ci

-- | Make a single-constructor product (struct)
mkProd ::
  () =>
  -- | encoding options
  Options ->
  -- | type name
  Name ->
  -- | haddock
  Maybe String ->
  -- | type variables
  [Type] ->
  -- | tags
  [Exp] ->
  -- | constructor info
  ConstructorInfo ->
  MoatM Match
mkProd o@Options {..} typName parentDoc instTys ts = \case
  -- single constructor, no fields
  ConstructorInfo
    { constructorVariant = NormalConstructor
    , constructorFields = []
    } -> do
      matchProxy =<< lift (structExp typName parentDoc instTys dataInterfaces dataProtocols dataAnnotations [] ts makeBase)
  -- single constructor, non-record (Normal)
  ConstructorInfo
    { constructorVariant = NormalConstructor
    , constructorName = name
    } -> do
      -- TODO: replace with 'value', ignore non-records
      -- instead of erroring. Make this configurable
      throwError $ SingleConNonRecord name
  -- single constructor, non-record (Infix)
  ConstructorInfo
    { constructorVariant = InfixConstructor
    , constructorName = name
    } -> do
      throwError $ EncounteredInfixConstructor name
  -- single constructor, record
  ConstructorInfo
    { constructorVariant = RecordConstructor fieldNames
    , ..
    } -> do
      fieldDocs <- lift $ mapM (getDocWith o) fieldNames
      fields <- zipFields o fieldNames constructorFields fieldDocs
      matchProxy =<< lift (structExp typName parentDoc instTys dataInterfaces dataProtocols dataAnnotations fields ts makeBase)

-- | 'strictFields' are required to exist in the record and are always included.
-- 'omitFields' will remove any remaining fields if they are 'Discard'ed.
zipFields :: Options -> [Name] -> [Type] -> [Maybe String] -> MoatM [Exp]
zipFields o ns ts ds = do
  let fields = nameStr <$> ns
      missingFields = strictFields o L.\\ fields
  if null missingFields
    then pure $ catMaybes $ zipWith3 mkField ns ts ds
    else throwError $ MissingStrictFields missingFields
  where
    mkField :: Name -> Type -> Maybe String -> Maybe Exp
    mkField n t d =
      let fieldStr = nameStr n
       in case (fieldStr `elem` strictFields o, omitFields o fieldStr) of
            (True, _) -> Just $ prettyField o n t d
            (False, Keep) -> Just $ prettyField o n t d
            (False, Discard) -> Nothing

-- turn a field name into a swift case name.
-- examples:
--
--   data Foo = A | B | C
--   =>
--   enum Foo {
--     case a
--     case b
--     case c
--   }
--
--   data Bar a = MkBar1 a | MkBar2
--   =>
--   enum Bar<A> {
--     case mkBar1(A)
--     case mkBar2
--   }
caseName :: Options -> Name -> Exp
caseName Options {..} =
  stringE
    . bool id (onHeadWith lowerFirstCase) constructorLowerFirst
    . constructorModifier
    . TS.unpack
    . last
    . TS.splitOn "."
    . TS.pack
    . show

-- remove qualifiers from a name, turn into String
nameStr :: Name -> String
nameStr = TS.unpack . last . TS.splitOn "." . TS.pack . show

-- remove qualifiers from a name, turn into Exp
unqualName :: Name -> Exp
unqualName = stringE . nameStr

-- prettify a type variable as an Exp
prettyTyVar :: Name -> Exp
prettyTyVar = stringE . map Char.toUpper . TS.unpack . head . TS.splitOn "_" . last . TS.splitOn "." . TS.pack . show

-- prettify a bunch of type variables as an Exp
prettyTyVars :: [Type] -> Exp
prettyTyVars = ListE . map prettyTyVar . getTyVars

-- get the free type variables from many types
getTyVars :: [Type] -> [Name]
getTyVars = mapMaybe getFreeTyVar

-- get the free type variables in a type
getFreeTyVar :: Type -> Maybe Name
getFreeTyVar = \case
  VarT name -> Just name
  SigT (VarT name) _kind -> Just name
  _ -> Nothing

-- make a struct field pretty
prettyField :: Options -> Name -> Type -> Maybe String -> Exp
prettyField Options {..} name ty doc =
  RecConE
    'Field
    [ ('fieldName, stringE (onHeadWith lowerFirstField (fieldLabelModifier (nameStr name))))
    , ('fieldType, toMoatTypeEPoly ty)
    , ('fieldDoc, prettyDoc doc)
    ]

-- prettify doc string as an Exp
prettyDoc :: Maybe String -> Exp
prettyDoc = \case
  Nothing -> ConE 'Nothing
  Just doc -> AppE (ConE 'Just) (stringE doc)

-- get doc string for Name
getDeclDoc :: Name -> Q (Maybe String)

#if MIN_VERSION_template_haskell(2,18,0)
getDeclDoc name = recover (pure Nothing) (getDoc (DeclDoc name))
#else
getDeclDoc _ = pure Nothing
#endif

-- get doc string if enabled in options
getDocWith :: Options -> Name -> Q (Maybe String)
getDocWith Options {..} n =
  if generateDocComments
    then getDeclDoc n
    else pure Nothing

-- build the instance head for a type
buildTypeInstance ::
  () =>
  -- | name of the type
  Name ->
  -- | which class instance head we are building
  Class ->
  -- | type variables
  [Type] ->
  -- | variant (datatype, newtype, data family, newtype family)
  DatatypeVariant ->
  MoatM (Cxt, Type)
buildTypeInstance tyConName cls varTysOrig variant = do
  -- Make sure to expand through type/kind synonyms!
  -- Otherwise, the eta-reduction check might get
  -- tripped up over type variables in a synonym
  -- that are actually dropped.
  -- (See GHC Trac #11416 for a scenario where this
  -- actually happened)
  varTysExp <- lift $ mapM resolveTypeSynonyms varTysOrig

  -- get the kind status of all of our types.
  -- we must realise them all to *.
  starKindStats :: [KindStatus] <-
    foldlM
      ( \stats k -> case canRealiseKindStar k of
          NotKindStar -> do
            throwError $ KindVariableCannotBeRealised tyConName k
          s -> pure (stats ++ [s])
      )
      []
      varTysExp

  let -- get the names of our kind vars
      kindVarNames :: [Name]
      kindVarNames =
        mapMaybe
          ( \case
              IsKindVar n -> Just n
              _ -> Nothing
          )
          starKindStats

  let -- instantiate polykinded things to star.
      varTysExpSubst :: [Type]
      varTysExpSubst = map (substNamesWithKindStar kindVarNames) varTysExp

      -- We now sub all of the specialised-to-* kind
      -- variable names with *, but in the original types,
      -- not the synonym-expanded types. The reason we
      -- do this is superficial: we want the derived
      -- instance to resemble the datatype written in
      -- source code as closely as possible. For example,
      --
      --   data family Fam a
      --   newtype instance Fam String = Fam String
      --
      -- We'd want to generate the instance:
      --
      --   instance C (Fam String)
      --
      -- Not:
      --
      --   instance C (Fam [Char])
      varTysOrigSubst :: [Type]
      varTysOrigSubst =
        map (substNamesWithKindStar kindVarNames) varTysOrig

      -- if we are working on a data family
      -- or newtype family, we need to peel off
      -- the kinds. See Note [Kind signatures in
      -- derived instances]
      varTysOrigSubst' :: [Type]
      varTysOrigSubst' =
        if isDataFamily variant
          then varTysOrigSubst
          else map unSigT varTysOrigSubst

      -- the class and type in the instance head.
      instanceType :: Type
      instanceType =
        AppT (ConT (moatClassName cls)) $
          applyTyCon tyConName varTysOrigSubst'

  -- the constraints needed on type variables
  -- makes up the constraint part of the
  -- instance head.
  instanceCxt <- catMaybes <$> mapM (deriveConstraint cls) varTysExpSubst

  -- forall <tys>. ctx tys => Cls ty
  pure (instanceCxt, instanceType)

-- the class we're generating an instance of
data Class
  = ClassType -- ToMoatType
  | ClassData -- ToMoatData

-- turn a 'Class' into a 'Name'
moatClassName :: Class -> Name
moatClassName = \case
  ClassType -> ''ToMoatType
  ClassData -> ''ToMoatData

-- derive the constraint needed on a type variable
-- in order to build the instance head for a class.
deriveConstraint ::
  () =>
  -- | class name
  Class ->
  -- | type
  Type ->
  -- | constraint on type
  MoatM (Maybe Pred)
deriveConstraint c@ClassType typ
  | not (isTyVar typ) = pure Nothing
  | hasKindStar typ = Just . applyCon (moatClassName c) <$> tName
  | otherwise = pure Nothing
  where
    tName :: MoatM Name
    tName = varTToName typ
    varTToName :: Type -> MoatM Name
    varTToName = \case
      VarT n -> pure n
      SigT t _ -> varTToName t
      t -> throwError $ EncounteredNonTypeVariable "deriveConstraint" t
deriveConstraint ClassData _ = pure Nothing

-- apply a type constructor to a type variable.
-- this can be useful for letting the kind
-- inference engine doing work for you. see
-- 'toMoatTypeECxt' for an example of this.
applyCon :: Name -> Name -> Pred
applyCon con t = AppT (ConT con) (VarT t)

-- peel off a kind signature from a Type
unSigT :: Type -> Type
unSigT = \case
  SigT t _ -> t
  t -> t

-- is the type a type variable?
isTyVar :: Type -> Bool
isTyVar = \case
  VarT _ -> True
  SigT t _ -> isTyVar t
  _ -> False

-- does the type have kind *?
hasKindStar :: Type -> Bool
hasKindStar = \case
  VarT _ -> True
  SigT _ StarT -> True
  _ -> False

-- perform the substitution of type variables
-- who have kinds which can be realised to *,
-- with the same type variable where its kind
-- has been turned into *
substNamesWithKindStar :: [Name] -> Type -> Type
substNamesWithKindStar ns t = foldr' (`substNameWithKind` starK) t ns
  where
    substNameWithKind :: Name -> Kind -> Type -> Type
    substNameWithKind n k = applySubstitution (M.singleton n k)

-- | The status of a kind variable w.r.t. its
--   ability to be realised into *.
data KindStatus
  = -- | kind * (or some k which can be realised to *)
    KindStar
  | -- | any other kind
    NotKindStar
  | -- | is actually a kind variable
    IsKindVar Name
  | -- | is a constructor - this will typically
    --   happen in a data family instance, because
    --   we often have to construct a
    --   FlexibleInstance. our old check for
    --   canRealiseKindStar didn't check for
    --   `ConT` - where this would happen.
    --
    --   TODO: Now i think this might need to be
    --   removed in favour of something smarter.
    IsCon Name

-- can we realise the type's kind to *?
canRealiseKindStar :: Type -> KindStatus
canRealiseKindStar = \case
  VarT {} -> KindStar
  SigT _ StarT -> KindStar
  SigT _ (VarT n) -> IsKindVar n
  ConT n -> IsCon n
  _ -> NotKindStar

-- fully applies a type constructor to its
-- type variables
applyTyCon :: Name -> [Type] -> Type
applyTyCon = foldl' AppT . ConT

-- Turn a String into an Exp string literal
stringE :: String -> Exp
stringE = LitE . StringL

-- convert a type into a 'Ty'.
-- we respect constraints here - e.g. in
-- `(Swift a, Swift b) => Swift (Foo a b)`,
-- we don't just fill in holes like in
-- `toMoatTypeEPoly`, we actually turn `a`
-- and `b` into `Ty`s directly. Consequently,
-- the implementation is much simpler - just
-- an application.
--
-- Note the use of unSigT - see Note
-- [Kind signatures in derived instances].
toMoatTypeECxt :: Type -> Exp
toMoatTypeECxt (unSigT -> typ) =
  AppE
    (VarE 'toMoatType)
    (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) typ))

-- convert a type into a 'Ty'.
-- polymorphic types do not require a 'ToMoatType'
-- instance, since we fill them in with 'SingSymbol'.
--
-- We do this by stretching out a type along its
-- spine, completely. we then fill in any polymorphic
-- variables with 'SingSymbol', reflecting the type
-- Name to a Symbol. then we compress the spine to
-- get the original type. the 'ToMoatType' instance for
-- 'SingSymbol' gets us where we need to go.
--
-- Note that @compress . decompress@ is not
-- actually equivalent to the identity function on
-- Type because of ForallT, where we discard some
-- context. However, for any types we care about,
-- there shouldn't be a ForallT, so this *should*
-- be fine.
toMoatTypeEPoly :: Type -> Exp
toMoatTypeEPoly = \case
  -- we don't need to special case VarT and SigT
  VarT n ->
    AppE (ConE 'Poly) (prettyTyVar n)
  SigT (VarT n) _ ->
    AppE (ConE 'Poly) (prettyTyVar n)
  typ ->
    let decompressed = decompress typ
        prettyName = map Char.toUpper . TS.unpack . head . TS.splitOn "_" . last . TS.splitOn "." . TS.pack . show
        filledInHoles =
          decompressed
            <&> ( \case
                    VarT name ->
                      AppT
                        (ConT ''Moat.SingSymbol)
                        (LitT (StrTyLit (prettyName name)))
                    SigT (VarT name) _ ->
                      AppT
                        (ConT ''Moat.SingSymbol)
                        (LitT (StrTyLit (prettyName name)))
                    t -> t
                )
        typ' = compress filledInHoles
     in AppE
          (VarE 'toMoatType)
          (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) typ'))

decompress :: Type -> Rose Type
decompress typ = case unapplyTy typ of
  tyCon :| tyArgs -> Rose tyCon (decompress <$> tyArgs)

compress :: Rose Type -> Type
compress (Rose typ []) = typ
compress (Rose t ts) = foldl' AppT t (compress <$> ts)

unapplyTy :: Type -> NonEmpty Type
unapplyTy = NE.reverse . go
  where
    go = \case
      AppT t1 t2 -> t2 <| go t1
      SigT t _ -> go t
      ForallT _ _ t -> go t
      t -> t :| []

-- | Types can be stretched out into a Rose tree.
--   decompress will stretch a type out completely,
--   in such a way that it cannot be stretched out
--   further. compress will reconstruct a type from
--   its stretched form.
--
--   Also note that this is equivalent to
--   Cofree NonEmpty Type.
--
--   Examples:
--
--   Maybe a
--   =>
--   AppT (ConT Maybe) (VarT a)
--
--
--   Either a b
--   =>
--   AppT (AppT (ConT Either) (VarT a)) (VarT b)
--   =>
--   Rose (ConT Either)
--     [ Rose (VarT a)
--         [
--         ]
--     , Rose (VarT b)
--         [
--         ]
--     ]
--
--
--   Either (Maybe a) (Maybe b)
--   =>
--   AppT (AppT (ConT Either) (AppT (ConT Maybe) (VarT a))) (AppT (ConT Maybe) (VarT b))
--   =>
--   Rose (ConT Either)
--     [ Rose (ConT Maybe)
--         [ Rose (VarT a)
--             [
--             ]
--         ]
--     , Rose (ConT Maybe)
--         [ Rose (VarT b)
--             [
--             ]
--         ]
--     ]
data Rose a = Rose a [Rose a]
  deriving stock (Eq, Show)
  deriving stock (Functor, Foldable, Traversable)

{-
Note [Kind signatures in derived instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible to put explicit kind signatures into the derived instances, e.g.,

  instance C a => C (Data (f :: * -> *)) where ...

But it is preferable to avoid this if possible. If we come up with an incorrect
kind signature (which is entirely possible, since Template Haskell doesn't always
have the best track record with reifying kind signatures), then GHC will flat-out
reject the instance, which is quite unfortunate.

Plain old datatypes have the advantage that you can avoid using any kind signatures
at all in their instances. This is because a datatype declaration uses all type
variables, so the types that we use in a derived instance uniquely determine their
kinds. As long as we plug in the right types, the kind inferencer can do the rest
of the work. For this reason, we use unSigT to remove all kind signatures before
splicing in the instance context and head.

Data family instances are trickier, since a data family can have two instances that
are distinguished by kind alone, e.g.,

  data family Fam (a :: k)
  data instance Fam (a :: * -> *)
  data instance Fam (a :: *)

If we dropped the kind signatures for C (Fam a), then GHC will have no way of
knowing which instance we are talking about. To avoid this scenario, we always
include explicit kind signatures in data family instances. There is a chance that
the inferred kind signatures will be incorrect, in which case we have to write the instance manually.
-}

-- are we working on a data family
-- or newtype family?
isDataFamily :: DatatypeVariant -> Bool
isDataFamily = \case
  NewtypeInstance -> True
  DataInstance -> True
  _ -> False

stripConT :: [Type] -> [Type]
stripConT = mapMaybe noConT
  where
    noConT = \case
      ConT {} -> Nothing
      t -> Just t

-- | Construct a Type Alias.
aliasExp ::
  () =>
  -- | alias name
  Name ->
  -- | haddock
  Maybe String ->
  -- | type variables
  [Type] ->
  -- | type (RHS)
  Type ->
  Exp
aliasExp name doc tyVars field =
  RecConE
    'MoatAlias
    [ ('aliasName, unqualName name)
    , ('aliasDoc, prettyDoc doc)
    , ('aliasTyVars, prettyTyVars tyVars)
    , ('aliasTyp, toMoatTypeECxt field)
    ]

-- | Construct a Tag.
tagExp ::
  () =>
  -- | tycon name
  Name ->
  -- | parent name
  Name ->
  -- | type of the tag (RHS)
  Type ->
  -- | Whether or not we are disambiguating.
  Bool ->
  Exp
tagExp tyconName parentName typ dis =
  RecConE
    'Tag
    [ ('tagName, unqualName tyconName)
    , ('tagParent, unqualName parentName)
    , ('tagTyp, toMoatTypeECxt typ)
    ,
      ( 'tagDisambiguate
      , if dis then ConE 'True else ConE 'False
      )
    ]

-- | Construct an Enum.
enumExp ::
  () =>
  -- | parent name
  Name ->
  -- | parent haddock
  Maybe String ->
  -- | type variables
  [Type] ->
  -- | interfaces
  [Interface] ->
  -- | protocols
  [Protocol] ->
  -- | annotations
  [Annotation] ->
  -- | cases
  [Exp] ->
  -- | Raw Value
  Maybe MoatType ->
  -- | Tags
  [Exp] ->
  -- | Make base?
  (Bool, Maybe MoatType, [Protocol]) ->
  SumOfProductEncodingOptions ->
  EnumEncodingStyle ->
  Q Exp
enumExp parentName parentDoc tyVars ifaces protos anns cases raw tags bs sop ees =
  do
    enumInterfaces_ <- Syntax.lift ifaces
    enumAnnotations_ <- Syntax.lift anns
    enumProtocols_ <- Syntax.lift protos
    sumOfProductEncodingOptions_ <- Syntax.lift sop
    enumEnumEncodingStyle_ <- Syntax.lift ees
    applyBase bs $
      RecConE
        'MoatEnum
        [ ('enumName, unqualName parentName)
        , ('enumDoc, prettyDoc parentDoc)
        , ('enumTyVars, prettyTyVars tyVars)
        , ('enumInterfaces, enumInterfaces_)
        , ('enumProtocols, enumProtocols_)
        , ('enumAnnotations, enumAnnotations_)
        , ('enumCases, ListE cases)
        , ('enumRawValue, rawValueE raw)
        , ('enumPrivateTypes, ListE [])
        , ('enumTags, ListE tags)
        , ('enumSumOfProductEncodingOption, sumOfProductEncodingOptions_)
        , ('enumEnumEncodingStyle, enumEnumEncodingStyle_)
        ]

newtypeExp ::
  () =>
  Name ->
  Maybe String ->
  [Type] ->
  [Interface] ->
  [Protocol] ->
  [Annotation] ->
  Exp ->
  Q Exp
newtypeExp name doc tyVars ifaces protos anns field =
  [|
    MoatNewtype
      { newtypeName = $(pure $ unqualName name)
      , newtypeDoc = $(pure $ prettyDoc doc)
      , newtypeTyVars = $(pure $ prettyTyVars tyVars)
      , newtypeField = $(pure field)
      , newtypeProtocols = $(Syntax.lift protos)
      , newtypeAnnotations = $(Syntax.lift anns)
      , newtypeInterfaces = $(Syntax.lift ifaces)
      }
    |]

-- | Construct a Struct.
structExp ::
  () =>
  -- | struct name
  Name ->
  -- | struct haddock
  Maybe String ->
  -- | type variables
  [Type] ->
  -- | interfaces
  [Interface] ->
  -- | protocols
  [Protocol] ->
  -- | annotations
  [Annotation] ->
  -- | fields
  [Exp] ->
  -- | tags
  [Exp] ->
  -- | Make base?
  (Bool, Maybe MoatType, [Protocol]) ->
  Q Exp
structExp name doc tyVars ifaces protos anns fields tags bs = do
  structInterfaces_ <- Syntax.lift ifaces
  structAnnotations_ <- Syntax.lift anns
  structProtocols_ <- Syntax.lift protos
  applyBase bs $
    RecConE
      'MoatStruct
      [ ('structName, unqualName name)
      , ('structDoc, prettyDoc doc)
      , ('structTyVars, prettyTyVars tyVars)
      , ('structInterfaces, structInterfaces_)
      , ('structProtocols, structProtocols_)
      , ('structAnnotations, structAnnotations_)
      , ('structFields, ListE fields)
      , ('structPrivateTypes, ListE [])
      , ('structTags, ListE tags)
      ]

matchProxy :: Exp -> MoatM Match
matchProxy e =
  lift $
    match
      (conP 'Proxy [])
      (normalB (pure e))
      []

stripFields :: MoatData -> MoatData
stripFields = \case
  s@MoatStruct {} -> s {structFields = []}
  s@MoatEnum {} -> s {enumCases = go (enumCases s)}
    where
      go = map stripOne
      stripOne (EnumCase name doc _) = EnumCase name doc []
  s -> s

giveProtos :: [Protocol] -> MoatData -> MoatData
giveProtos ps = \case
  s@MoatStruct {} -> s {structProtocols = ps}
  s@MoatEnum {} -> s {enumProtocols = ps}
  s -> s

suffixBase :: MoatData -> MoatData
suffixBase = \case
  s@MoatStruct {} -> s {structName = structName s ++ "Base"}
  s@MoatEnum {} -> s {enumName = enumName s ++ "Base"}
  s -> s

giveBase :: Maybe MoatType -> [Protocol] -> MoatData -> MoatData
giveBase r ps = \case
  s@MoatStruct {} -> s {structPrivateTypes = [giveProtos ps (suffixBase (stripFields s))]}
  s@MoatEnum {} ->
    case giveProtos ps (suffixBase (stripFields s)) of
      result@MoatEnum {} -> s {enumPrivateTypes = [result {enumRawValue = r}]}
      _ -> s
  s -> s

-- | Apply 'giveBase' to a 'MoatData'.
--
--   Ideally we would offload this into
--   the first construction of the MoatData,
--   inside structExp/enumExp.
--
--
-- should we strip tyvars as well?
applyBase :: (Bool, Maybe MoatType, [Protocol]) -> Exp -> Q Exp
applyBase (b, r, ps) (ParensE -> s) = do
  protoAnnotations <- Syntax.lift ps
  pure $
    if b
      then VarE 'giveBase `AppE` rawValueE r `AppE` protoAnnotations `AppE` s
      else s

-- | Convert moat aliases to moat newtypes
aliasToNewtype :: MoatData -> MoatData
aliasToNewtype MoatAlias {..} =
  MoatNewtype
    { newtypeName = aliasName
    , newtypeTyVars = aliasTyVars
    , newtypeField = Field {fieldName = "value", fieldType = aliasTyp, fieldDoc = Nothing}
    , newtypeInterfaces = []
    , newtypeProtocols = []
    , newtypeAnnotations = []
    , newtypeDoc = aliasDoc
    }
aliasToNewtype m = m

-- | Convert moat newtypes to moat aliases
newtypeToAlias :: MoatData -> MoatData
newtypeToAlias MoatNewtype {..} =
  MoatAlias
    { aliasName = newtypeName
    , aliasTyVars = newtypeTyVars
    , aliasTyp = fieldType newtypeField
    , aliasDoc = newtypeDoc
    }
newtypeToAlias m = m
