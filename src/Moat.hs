{-# language
    AllowAmbiguousTypes
  , BangPatterns
  , CPP
  , DataKinds
  , DeriveFoldable
  , DeriveFunctor
  , DeriveGeneric
  , DeriveTraversable
  , DerivingStrategies
  , FlexibleInstances
  , LambdaCase
  , MultiWayIf
  , NamedFieldPuns
  , OverloadedStrings
  , RecordWildCards
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
  , ViewPatterns
#-}

{-# options_ghc
  -Wall
#-}

-- | The Shwifty library allows generation of
--   Swift types (structs and enums) from Haskell
--   ADTs, using Template Haskell. The main
--   entry point to the library should be the
--   documentation and examples of 'mobileGen'.
--   See also 'mobileGenWith'
--   and 'mobileGenWithTags'.
--
--   This library is in alpha and there are a number
--   of known bugs which shouldn't affect most users.
--   See the issue tracker to see what those are.
--
--   There are probably many bugs/some weird behaviour
--   when it comes to data families. Please report
--   any issues on the issue tracker.
module Moat
  ( -- * Classes for conversion
    ToMoatType(..)
  , ToMoatData(..)

    -- * Generating instances
  , mobileGen
  , mobileGenWith
  , mobileGenWithTags

    -- * Types
  , MoatType(..)
  , MoatData(..)
  , Protocol(..)

    -- * Options for encoding types
    -- ** Option type
  , Options
    -- ** Helper type for omissions
  , KeepOrDiscard(..)
    -- ** Actual Options
  , fieldLabelModifier
  , constructorModifier
  , optionalExpand
  , generateToMoatType
  , generateToMoatData
  , dataProtocols
  , dataRawValue
  , typeAlias
  , newtypeTag
  , lowerFirstCase
  , lowerFirstField
  , omitFields
  , omitCases
  , makeBase
    -- ** Default 'Options'
  , defaultOptions

    -- * Pretty-printing
    -- ** Functions
  , prettyKotlinData
  , prettySwiftData
    -- ** Re-exports
  , X
  ) where

import Control.Monad.Except
import Data.Foldable (foldlM,foldr',foldl')
import Data.Functor ((<&>))
import Data.List.NonEmpty ((<|), NonEmpty(..))
import Data.Maybe (mapMaybe, catMaybes)
import Data.Proxy (Proxy(..))
import Data.Void (Void)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Language.Haskell.TH hiding (stringE, tupE)
import Language.Haskell.TH.Datatype
import Prelude hiding (Enum(..))
import qualified Data.Char as Char
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as TS

import Moat.Class
import Moat.Pretty.Kotlin (prettyKotlinData)
import Moat.Pretty.Swift (prettySwiftData)
import Moat.Types

-- Used internally to reflect polymorphic type
-- variables into TH, then reify them into 'Poly'.
--
-- See the Rose tree section below
data SingSymbol (x :: Symbol)
instance KnownSymbol x => ToMoatType (SingSymbol x) where
  toMoatType _ = Poly (symbolVal (Proxy @x))

-- | A filler type to be used when pretty-printing.
--   The codegen used by shwifty doesn't look at
--   at what a type's type variables are instantiated
--   to, but rather at the type's top-level
--   definition. However,
--   to make GHC happy, you will have to fill in type
--   variables with unused types. To get around this,
--   you could also use something like
--   `-XQuantifiedConstraints`, or existential types,
--   but we leave that to the user to handle.
type X = Void

ensureEnabled :: Extension -> ShwiftyM ()
ensureEnabled ext = do
  enabled <- lift $ isExtEnabled ext
  unless enabled $ do
    throwError $ ExtensionNotEnabled ext

-- | Generate 'ToMoatData' and 'ToMoatType' instances
--   for your type. 'ToMoatType' instances are typically
--   used to build cases or fields, whereas
--   'ToMoatData' instances are for building structs
--   and enums. Click the @Examples@ button to see
--   examples of what Swift gets generated in
--   different scenarios. To get access to the
--   generated code, you will have to use one of
--   the pretty-printing functions provided.
--
-- === __Examples__
--
-- > -- A simple sum type
-- > data SumType = Sum1 | Sum2 | Sum3
-- > mobileGen ''SumType
--
-- @
-- enum SumType {
--     case sum1
--     case sum2
--     case sum3
-- }
-- @
--
-- > -- A simple product type
-- > data ProductType = ProductType { x :: Int, y :: Int }
-- > mobileGen ''ProductType
--
-- @
-- struct ProductType {
--     let x: Int
--     let y: Int
-- }
-- @
--
-- > -- A sum type with type variables
-- > data SumType a b = SumL a | SumR b
-- > mobileGen ''SumType
--
-- @
-- enum SumType\<A, B\> {
--     case sumL(A)
--     case sumR(B)
-- }
-- @
--
-- > -- A product type with type variables
-- > data ProductType a b = ProductType { aField :: a, bField :: b }
-- > mobileGen ''ProductType
--
-- @
-- struct ProductType\<A, B\> {
--     let aField: A
--     let bField: B
-- }
-- @
--
-- > -- A newtype
-- > newtype Newtype a = Newtype { getNewtype :: a }
-- > mobileGen ''Newtype
--
-- @
-- struct Newtype\<A\> {
--     let getNewtype: A
-- }
-- @
--
-- > -- A type with a function field
-- > newtype Endo a = Endo { appEndo :: a -> a }
-- > mobileGen ''Endo
--
-- @
-- struct Endo\<A\> {
--     let appEndo: ((A) -> A)
-- }
-- @
--
-- > -- A type with a kookier function field
-- > newtype Fun a = Fun { fun :: Int -> Char -> Bool -> String -> Maybe a }
-- > mobileGen ''Fun
--
-- @
-- struct Fun\<A\> {
--     let fun: ((Int, Char, Bool, String) -> A?)
-- }
-- @
--
-- > -- A weird type with nested fields. Also note the Result's types being flipped from that of the Either.
-- > data YouveGotProblems a b = YouveGotProblems { field1 :: Maybe (Maybe (Maybe a)), field2 :: Either (Maybe a) (Maybe b) }
-- > mobileGen ''YouveGotProblems
--
-- @
-- struct YouveGotProblems\<A, B\> {
--     let field1: Option\<Option\<Option\<A\>\>\>
--     let field2: Result\<Option\<B\>,Option\<A\>\>
-- }
-- @
--
-- > -- A type with polykinded type variables
-- > -- Also note that there is no newline because
-- > -- of the absence of fields
-- > data PolyKinded (a :: k) = PolyKinded
-- > mobileGen ''PolyKinded
--
-- @
-- struct PolyKinded\<A\> { }
-- @
--
-- > -- A sum type where constructors might be records
-- > data SumType a b (c :: k) = Sum1 Int a (Maybe b) | Sum2 b | Sum3 { x :: Int, y :: Int }
-- > mobileGen ''SumType
--
-- @
-- enum SumType\<A, B, C\> {
--   case field1(Int, A, Optional\<B\>)
--   case field2(B)
--   case field3(_ x: Int, _ y: Int)
-- }
-- @
--
-- > -- A type containing another type with instance generated by 'mobileGen'
-- > newtype MyFirstType a = MyFirstType { getMyFirstType :: a }
-- > mobileGen ''MyFirstType
-- >
-- > data Contains a = Contains { x :: MyFirstType Int, y :: MyFirstType a }
-- > mobileGen ''Contains
--
-- @
-- struct MyFirstType\<A\> {
--   let getMyFirstType: A
-- }
--
-- struct Contains\<A\> {
--   let x: MyFirstType\<Int\>
--   let y: MyFirstType\<A\>
-- }
-- @
mobileGen :: Name -> Q [Dec]
mobileGen = mobileGenWith defaultOptions

-- | Like 'mobileGen', but lets you supply
--   your own 'Options'. Click the examples
--   for some clarification of what you can do.
--
-- === __Examples__
--
-- > data PrefixedFields = MkPrefixedFields { prefixedFieldsX :: Int, prefixedFieldsY :: Int }
-- > $(mobileGenWith (defaultOptions { fieldLabelModifier = drop (length "PrefixedFields") }) ''PrefixedFields)
--
-- @
-- struct PrefixedFields {
--     let x: Int
--     let y: Int
-- }
-- @
--
-- > data PrefixedCons = MkPrefixedConsLeft | MkPrefixedConsRight
-- > $(mobileGenWith (defaultOptions { constructorModifier = drop (length "MkPrefixedCons"), dataProtocols = [Codable] }) ''PrefixedCons)
--
-- @
-- enum PrefixedCons: Codable {
--     case left
--     case right
-- }
-- @
mobileGenWith :: Options -> Name -> Q [Dec]
mobileGenWith o n = mobileGenWithTags o [] n

data NewtypeInfo = NewtypeInfo
  { newtypeName :: Name
    -- ^ Type constructor
  , newtypeVars :: [TyVarBndr]
    -- ^ Type parameters
  , newtypeInstTypes :: [Type]
    -- ^ Argument types
  , newtypeVariant :: DatatypeVariant
    -- ^ Whether or not the type is a
    --   newtype or newtype instance
  , newtypeCon :: ConstructorInfo
  }

-- | Reify a newtype.
reifyNewtype :: Name -> ShwiftyM NewtypeInfo
reifyNewtype n = do
  DatatypeInfo{..} <- lift $ reifyDatatype n
  case (datatypeCons, datatypeVariant) of
    ([c], Newtype) -> do
      pure NewtypeInfo {
        newtypeName = datatypeName
      , newtypeVars = datatypeVars
      , newtypeInstTypes = datatypeInstTypes
      , newtypeVariant = datatypeVariant
      , newtypeCon = c
      }
    ([c], NewtypeInstance) -> do
      pure NewtypeInfo {
        newtypeName = datatypeName
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
getTags :: ()
  => Name
     -- ^ name of parent type
  -> [Name]
     -- ^ tags
  -> ShwiftyM ([Exp], [Dec])
getTags parentName ts = do
  let b = length ts > 1
  disambiguate <- lift $ [||b||]
  tags <- foldlM
    (\(es,ds) n -> do

      NewtypeInfo{..} <- reifyNewtype n
      let ConstructorInfo{..} = newtypeCon

      -- generate the tag
      let tyconName = case newtypeVariant of
            NewtypeInstance -> constructorName
            _ -> newtypeName
      typ <- case constructorFields of
        [ty] -> pure ty
        _ -> throwError $ NotANewtype newtypeName
      let tag = RecConE 'Tag
            [ (mkName "tagName", unqualName tyconName)
            , (mkName "tagParent", unqualName parentName)
            , (mkName "tagTyp", toMoatTypeEPoly typ)
            , (mkName "tagDisambiguate", unType disambiguate)
            ]

      -- generate the instance
      !instHeadTy
        <- buildTypeInstance newtypeName ClassType newtypeInstTypes newtypeVars newtypeVariant
      -- we do not want to strip here
      clauseTy <- tagToMoatType tyconName typ parentName
      swiftTyInst <- lift $ instanceD
        (pure [])
        (pure instHeadTy)
        [ funD 'toMoatType
          [ clause [] (normalB (pure clauseTy)) []
          ]
        ]

      pure $ (es ++ [tag], ds ++ [swiftTyInst])
    ) ([], []) ts
  pure tags

getToMoatType :: ()
  => Options
     -- ^ options
  -> Name
     -- ^ type name
  -> [Type]
     -- ^ type variables
  -> [TyVarBndr]
     -- ^ type binders
  -> DatatypeVariant
     -- ^ type variant
  -> [ConstructorInfo]
     -- ^ constructors
  -> ShwiftyM [Dec]
getToMoatType Options{..} parentName instTys tyVarBndrs variant cons = if generateToMoatType
  then do
    instHead <- buildTypeInstance parentName ClassType instTys tyVarBndrs variant
    clauseTy <- case variant of
      NewtypeInstance -> case cons of
        [ConstructorInfo{..}] -> do
          newtypToMoatType constructorName instTys
        _ -> do
          throwError ExpectedNewtypeInstance
      _ -> do
        typToMoatType newtypeTag parentName instTys
    inst <- lift $ instanceD
      (pure [])
      (pure instHead)
      [ funD 'toMoatType
        [ clause [] (normalB (pure clauseTy)) []
        ]
      ]
    pure [inst]
  else do
    pure []

getToMoatData :: ()
  => Options
     -- ^ options
  -> Name
     -- ^ type name
  -> [Type]
     -- ^ type variables
  -> [TyVarBndr]
     -- ^ type binders
  -> DatatypeVariant
     -- ^ type variant
  -> [Exp]
     -- ^ tags
  -> [ConstructorInfo]
     -- ^ constructors
  -> ShwiftyM [Dec]
getToMoatData o@Options{..} parentName instTys tyVarBndrs variant tags cons =
  if generateToMoatData
  then do
    instHead <- buildTypeInstance parentName ClassData instTys tyVarBndrs variant
    clauseData <- consToMoatType o parentName instTys variant tags makeBase cons
    inst <- lift $ instanceD
      (pure [])
      (pure instHead)
        [ funD 'toMoatData
          [ clause [] (normalB (pure clauseData)) []
          ]
        ]
    pure [inst]
  else do
    pure []

-- | Like 'mobileGenWith', but lets you supply
--   tags. Tags are type-safe typealiases that
--   are akin to newtypes in Haskell. The
--   introduction of a struct around something
--   which is, say, a UUID in Swift means that
--   the default Codable instance will not work
--   correctly. So we introduce a tag(s). See the
--   examples to see how this looks. Also, see
--   https://github.com/pointfreeco/swift-tagged,
--   the library which these tags use. The library
--   is not included in any generated code.
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
-- /Note/: Tags are ignored on the Kotlin
-- backend.
mobileGenWithTags :: ()
  => Options
  -> [Name]
  -> Name
  -> Q [Dec]
mobileGenWithTags o ts name = do
  r <- runExceptT $ do
    ensureEnabled ScopedTypeVariables
    ensureEnabled DataKinds
    DatatypeInfo
      { datatypeName = parentName
      , datatypeVars = tyVarBndrs
      , datatypeInstTypes = instTys
      , datatypeVariant = variant
      , datatypeCons = cons
      } <- lift $ reifyDatatype name
    noExistentials cons

    -- get tags/ToMoatType instances for tags
    (tags, extraDecs) <- getTags parentName ts

    swiftDataInst <- getToMoatData o parentName instTys tyVarBndrs variant tags cons

    swiftTyInst <- getToMoatType o parentName instTys tyVarBndrs variant cons
    pure $ swiftDataInst ++ swiftTyInst ++ extraDecs
  case r of
    Left e -> fail $ prettyShwiftyError e
    Right d -> pure d

noExistentials :: [ConstructorInfo] -> ShwiftyM ()
noExistentials cs = forM_ cs $ \ConstructorInfo{..} ->
  case (constructorName, constructorVars) of
    (_, []) -> do
      pure ()
    (cn, cvs) -> do
      throwError $ ExistentialTypes cn cvs

data ShwiftyError
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

prettyShwiftyError :: ShwiftyError -> String
prettyShwiftyError = \case
  SingleConNonRecord (nameStr -> n) -> mempty
    ++ n
    ++ ": Cannot get shwifty with single-constructor "
    ++ "non-record types. This is due to a "
    ++ "restriction of Swift that prohibits structs "
    ++ "from not having named fields. Try turning "
    ++ n ++ " into a record!"
  EncounteredInfixConstructor (nameStr -> n) -> mempty
    ++ n
    ++ ": Cannot get shwifty with infix constructors. "
    ++ "Swift doesn't support them. Try changing "
    ++ n ++ " into a prefix constructor!"
  KindVariableCannotBeRealised (nameStr -> n) typ ->
    let (typStr, kindStr) = prettyKindVar typ
    in mempty
      ++ n
      ++ ": Encountered a type variable ("
      ++ typStr
      ++ ") with a kind ("
      ++ kindStr
      ++ ") that can't "
      ++ "get shwifty! Shwifty needs to be able "
      ++ "to realise your kind variables to `*`, "
      ++ "since that's all that makes sense in "
      ++ "Swift. The only kinds that can happen with "
      ++ "are `*` and the free-est kind, `k`."
  ExtensionNotEnabled ext -> mempty
    ++ show ext
    ++ " is not enabled. Shwifty needs it to work!"
  -- TODO: make this not print out implicit kinds.
  -- e.g. for `data Ex = forall x. Ex x`, there are
  -- no implicit `TyVarBndr`s, but for
  -- `data Ex = forall x y z. Ex x`, there are two:
  -- the kinds inferred by `y` and `z` are both `k`.
  -- We print these out - this could be confusing to
  -- the end user. I'm not immediately certain how to
  -- be rid of them.
  ExistentialTypes (nameStr -> n) tys -> mempty
    ++ n
    ++ " has existential type variables ("
    ++ L.intercalate ", " (map prettyTyVarBndrStr tys)
    ++ ")! Shwifty doesn't support these."
  ExpectedNewtypeInstance -> mempty
    ++ "Expected a newtype instance. This is an "
    ++ "internal logic error. Please report it as a "
    ++ "bug."
  NotANewtype (nameStr -> n) -> mempty
    ++ n
    ++ " is not a newtype. This is an internal logic "
    ++ "error. Please report it as a bug."

prettyTyVarBndrStr :: TyVarBndr -> String
prettyTyVarBndrStr = \case
  PlainTV n -> go n
  KindedTV n _ -> go n
  where
    go = TS.unpack . head . TS.splitOn "_" . last . TS.splitOn "." . TS.pack . show

-- prettify the type and kind.
prettyKindVar :: Type -> (String, String)
prettyKindVar = \case
  SigT typ k -> (go typ, go k)
  VarT n -> (nameStr n, "*")
  typ -> error $ "Shwifty.prettyKindVar: used on a type without a kind signature. Type was: " ++ show typ
  where
    go = TS.unpack . head . TS.splitOn "_" . last . TS.splitOn "." . TS.pack . show . ppr

type ShwiftyM = ExceptT ShwiftyError Q

tagToMoatType :: ()
  => Name
     -- ^ name of the type constructor
  -> Type
     -- ^ type variables
  -> Name
     -- ^ parent name
  -> ShwiftyM Exp
tagToMoatType tyconName typ parentName = do
  -- TODO: use '_' instead of matching
  value <- lift $ newName "value"
  ourMatch <- matchProxy
    $ tagExp tyconName parentName typ False
  let matches = [pure ourMatch]
  lift $ lamE [varP value] (caseE (varE value) matches)
newtypToMoatType :: ()
  => Name
     -- ^ name of the constructor
  -> [Type]
     -- ^ type variables
  -> ShwiftyM Exp
newtypToMoatType conName (stripConT -> instTys) = do
  typToMoatType False conName instTys

typToMoatType :: ()
  => Bool
     -- ^ is this a newtype tag?
  -> Name
     -- ^ name of the type
  -> [Type]
     -- ^ type variables
  -> ShwiftyM Exp
typToMoatType newtypeTag parentName instTys = do
  -- TODO: use '_' instead of matching
  value <- lift $ newName "value"
  let tyVars = map toMoatTypeECxt instTys
  let name =
        let parentStr = nameStr parentName
            accessedName = if newtypeTag
              then parentStr ++ "Tag." ++ parentStr
              else parentStr
        in stringE accessedName
  ourMatch <- matchProxy
    $ RecConE 'Concrete
    $ [ (mkName "concreteName", name)
      , (mkName "concreteTyVars", ListE tyVars)
      ]
  let matches = [pure ourMatch]
  lift $ lamE [varP value] (caseE (varE value) matches)

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
  Tag{..} -> AppE (AppE (AppE (AppE (ConE 'Tag) (stringE tagName)) (stringE tagParent)) (tyE tagTyp)) (if tagDisambiguate then ConE 'True else ConE 'False)

consToMoatType :: ()
  => Options
     -- ^ options about how to encode things
  -> Name
     -- ^ name of type
  -> [Type]
     -- ^ type variables
  -> DatatypeVariant
     -- ^ data type variant
  -> [Exp]
     -- ^ tags
  -> (Bool, Maybe MoatType, [Protocol])
     -- ^ Make base?
  -> [ConstructorInfo]
     -- ^ constructors
  -> ShwiftyM Exp
consToMoatType o@Options{..} parentName instTys variant ts bs = \case
  [] -> do
    value <- lift $ newName "value"
    matches <- liftCons (mkVoid parentName instTys ts)
    lift $ lamE [varP value] (caseE (varE value) matches)
  cons -> do
    -- TODO: use '_' instead of matching
    value <- lift $ newName "value"
    matches <- matchesWorker
    lift $ lamE [varP value] (caseE (varE value) matches)
    where
      -- bad name
      matchesWorker :: ShwiftyM [Q Match]
      matchesWorker = case cons of
        [con] -> liftCons $ do
          case variant of
            NewtypeInstance -> do
              if | typeAlias -> do
                     mkNewtypeInstanceAlias instTys con
                 | otherwise -> do
                     mkNewtypeInstance o instTys ts con
            Newtype -> do
              if | newtypeTag -> do
                     mkTypeTag o parentName instTys con
                 | typeAlias -> do
                     mkTypeAlias parentName instTys con
                 | otherwise -> do
                     mkNewtype o parentName instTys con
            _ -> do
              mkProd o parentName instTys ts con
        _ -> do
          -- omit the cases we don't want
          let cons' = flip filter cons $ \ConstructorInfo{..} -> omitCases (nameStr constructorName) == Keep
          cases <- forM cons' (liftEither . mkCase o)
          ourMatch <- matchProxy
            $ enumExp parentName instTys dataInterfaces dataProtocols cases dataRawValue ts bs
          pure [pure ourMatch]

liftCons :: (Functor f, Applicative g) => f a -> f ([g a])
liftCons x = ((:[]) . pure) <$> x

-- Create the case (String, [(Maybe String, Ty)])
mkCaseHelper :: Options -> Name -> [Exp] -> Exp
mkCaseHelper o name es = tupE [ caseName o name, ListE es ]

mkCase :: ()
  => Options
  -> ConstructorInfo
  -> Either ShwiftyError Exp
mkCase o = \case
  -- non-record
  ConstructorInfo
    { constructorVariant = NormalConstructor
    , constructorName = name
    , constructorFields = fields
    } -> Right $ mkCaseHelper o name $ fields <&>
        (\typ -> tupE
          [ ConE 'Nothing
          , toMoatTypeEPoly typ
          ]
        )
  ConstructorInfo
    { constructorVariant = InfixConstructor
    , constructorName = name
    } -> Left $ EncounteredInfixConstructor name
  -- records
  -- we turn names into labels
  ConstructorInfo
    { constructorVariant = RecordConstructor fieldNames
    , constructorName = name
    , constructorFields = fields
    } ->
       let cases = zipWith (caseField o) fieldNames fields
       in Right $ mkCaseHelper o name cases

caseField :: Options -> Name -> Type -> Exp
caseField o n typ = tupE
  [ mkLabel o n
  , toMoatTypeEPoly typ
  ]

onHeadWith :: Bool -> String -> String
onHeadWith toLower = if toLower
  then onHead Char.toLower
  else id

-- apply a function only to the head of a string
onHead :: (Char -> Char) -> String -> String
onHead f = \case { [] -> []; (x:xs) -> f x : xs }

mkLabel :: Options -> Name -> Exp
mkLabel Options{..} = AppE (ConE 'Just)
  . stringE
  . fieldLabelModifier
  . onHeadWith lowerFirstField
  . TS.unpack
  . last
  . TS.splitOn "."
  . TS.pack
  . show

mkNewtypeInstanceAlias :: ()
  => [Type]
     -- ^ type variables
  -> ConstructorInfo
     -- ^ constructor info
  -> ShwiftyM Match
mkNewtypeInstanceAlias (stripConT -> instTys) = \case
  ConstructorInfo
    { constructorName = conName
    , constructorFields = [field]
    } -> do
      lift $ match
        (conP 'Proxy [])
        (normalB
          (pure
            (aliasExp conName instTys field)))
        []
  _ -> throwError $ ExpectedNewtypeInstance

mkNewtypeInstance :: ()
  => Options
     -- ^ encoding options
  -> [Type]
     -- ^ type variables
  -> [Exp]
     -- ^ tags
  -> ConstructorInfo
     -- ^ constructor info
  -> ShwiftyM Match
mkNewtypeInstance o@Options{..} (stripConT -> instTys) ts = \case
  ConstructorInfo
    { constructorVariant = RecordConstructor [fieldName]
    , constructorFields = [field]
    , ..
    } -> do
      let fields = [prettyField o fieldName field]
      matchProxy $ structExp constructorName instTys dataInterfaces dataProtocols fields ts makeBase
  _ -> throwError ExpectedNewtypeInstance

-- make a newtype into an empty enum
-- with a tag
mkTypeTag :: ()
  => Options
     -- ^ options
  -> Name
     -- ^ type name
  -> [Type]
     -- ^ type variables
  -> ConstructorInfo
     -- ^ constructor info
  -> ShwiftyM Match
mkTypeTag Options{..} typName instTys = \case
  ConstructorInfo
    { constructorFields = [field]
    } -> do
      let parentName = mkName
            (nameStr typName ++ "Tag")
      let tag = tagExp typName parentName field False
      matchProxy $ enumExp parentName instTys dataInterfaces dataProtocols [] dataRawValue [tag] (False, Nothing, [])

  _ -> throwError $ NotANewtype typName

-- make a newtype into a type alias
mkTypeAlias :: ()
  => Name
     -- ^ type name
  -> [Type]
     -- ^ type variables
  -> ConstructorInfo
     -- ^ constructor info
  -> ShwiftyM Match
mkTypeAlias typName instTys = \case
  ConstructorInfo
    { constructorFields = [field]
    } -> do
      lift $ match
        (conP 'Proxy [])
        (normalB
          (pure (aliasExp typName instTys field)))
        []
  _ -> throwError $ NotANewtype typName

-- | Make a void type (empty enum)
mkVoid :: ()
  => Name
     -- ^ type name
  -> [Type]
     -- ^ type variables
  -> [Exp]
     -- ^ tags
  -> ShwiftyM Match
mkVoid typName instTys ts = matchProxy
  $ enumExp typName instTys [] [] [] Nothing ts (False, Nothing, [])

mkNewtype :: ()
  => Options
  -> Name
  -> [Type]
  -> ConstructorInfo
  -> ShwiftyM Match
mkNewtype o@Options{..} typName instTys = \case
  -- TODO: make this only accept proper con variants
  ConstructorInfo
    { constructorFields = [field]
    } -> do
      matchProxy $ newtypeExp typName instTys dataInterfaces dataProtocols (prettyField o (mkName "value") field)
  _ -> undefined

-- | Make a single-constructor product (struct)
mkProd :: ()
  => Options
     -- ^ encoding options
  -> Name
     -- ^ type name
  -> [Type]
     -- ^ type variables
  -> [Exp]
     -- ^ tags
  -> ConstructorInfo
     -- ^ constructor info
  -> ShwiftyM Match
mkProd o@Options{..} typName instTys ts = \case
  -- single constructor, no fields
  ConstructorInfo
    { constructorVariant = NormalConstructor
    , constructorFields = []
    } -> do
      matchProxy $ structExp typName instTys dataInterfaces dataProtocols [] ts makeBase
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
      let fields = zipFields o fieldNames constructorFields
      matchProxy $ structExp typName instTys dataInterfaces dataProtocols fields ts makeBase

zipFields :: Options -> [Name] -> [Type] -> [Exp]
zipFields o = zipWithPred p (prettyField o)
  where
    p :: Name -> Type -> Bool
    p n _ = omitFields o (nameStr n) == Keep

zipWithPred :: (a -> b -> Bool) -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithPred _ _ [] _ = []
zipWithPred _ _ _ [] = []
zipWithPred p f (x:xs) (y:ys)
  | p x y = f x y : zipWithPred p f xs ys
  | otherwise = zipWithPred p f xs ys

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
caseName Options{..} = id
  . stringE
  . onHeadWith lowerFirstCase
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
prettyField :: Options -> Name -> Type -> Exp
prettyField Options{..} name ty = tupE
  [ (stringE (onHeadWith lowerFirstField (fieldLabelModifier (nameStr name))))
  , toMoatTypeEPoly ty
  ]

-- build the instance head for a type
buildTypeInstance :: ()
  => Name
     -- ^ name of the type
  -> Class
     -- ^ which class instance head we are building
  -> [Type]
     -- ^ type variables
  -> [TyVarBndr]
     -- ^ the binders for our tyvars
  -> DatatypeVariant
     -- ^ variant (datatype, newtype, data family, newtype family)
  -> ShwiftyM Type
buildTypeInstance tyConName cls varTysOrig tyVarBndrs variant = do
  -- Make sure to expand through type/kind synonyms!
  -- Otherwise, the eta-reduction check might get
  -- tripped up over type variables in a synonym
  -- that are actually dropped.
  -- (See GHC Trac #11416 for a scenario where this
  -- actually happened)
  varTysExp <- lift $ mapM resolveTypeSynonyms varTysOrig

  -- get the kind status of all of our types.
  -- we must realise them all to *.
  starKindStats :: [KindStatus] <- foldlM
    (\stats k -> case canRealiseKindStar k of
      NotKindStar -> do
        throwError $ KindVariableCannotBeRealised tyConName k
      s -> pure (stats ++ [s])
    ) [] varTysExp

  let -- get the names of our kind vars
      kindVarNames :: [Name]
      kindVarNames = flip mapMaybe starKindStats
        (\case
            IsKindVar n -> Just n
            _ -> Nothing
        )

  let
      -- instantiate polykinded things to star.
      varTysExpSubst :: [Type]
      varTysExpSubst = map (substNamesWithKindStar kindVarNames) varTysExp

      -- the constraints needed on type variables
      preds :: [Maybe Pred]
      preds = map (deriveConstraint cls) varTysExpSubst

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
        map (substNamesWithKindStar kindVarNames) $ varTysOrig

      -- if we are working on a data family
      -- or newtype family, we need to peel off
      -- the kinds. See Note [Kind signatures in
      -- derived instances]
      varTysOrigSubst' :: [Type]
      varTysOrigSubst' = if isDataFamily variant
        then varTysOrigSubst
        else map unSigT varTysOrigSubst

      -- the constraints needed on type variables
      -- makes up the constraint part of the
      -- instance head.
      instanceCxt :: Cxt
      instanceCxt = catMaybes preds

      -- the class and type in the instance head.
      instanceType :: Type
      instanceType = AppT (ConT (shwiftyClassName cls))
        $ applyTyCon tyConName varTysOrigSubst'

  -- forall <tys>. ctx tys => Cls ty
  lift $ forallT
    (map tyVarBndrNoSig tyVarBndrs)
    (pure instanceCxt)
    (pure instanceType)

-- the class we're generating an instance of
data Class
  = ClassType -- ToMoatType
  | ClassData -- ToMoatData

-- turn a 'Class' into a 'Name'
shwiftyClassName :: Class -> Name
shwiftyClassName = \case
  ClassType -> ''ToMoatType
  ClassData -> ''ToMoatData

-- derive the constraint needed on a type variable
-- in order to build the instance head for a class.
deriveConstraint :: ()
  => Class
     -- ^ class name
  -> Type
     -- ^ type
  -> Maybe Pred
     -- ^ constraint on type
deriveConstraint c@ClassType typ
  | not (isTyVar typ) = Nothing
  | hasKindStar typ = Just (applyCon (shwiftyClassName c) tName)
  | otherwise = Nothing
  where
    tName :: Name
    tName = varTToName typ
    varTToName = \case
      VarT n -> n
      SigT t _ -> varTToName t
      _ -> error "Shwifty.varTToName: encountered non-type variable"
deriveConstraint ClassData _ = Nothing

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
  = KindStar
    -- ^ kind * (or some k which can be realised to *)
  | NotKindStar
    -- ^ any other kind
  | IsKindVar Name
    -- ^ is actually a kind variable
  | IsCon Name
    -- ^ is a constructor - this will typically
    --   happen in a data family instance, because
    --   we often have to construct a
    --   FlexibleInstance. our old check for
    --   canRealiseKindStar didn't check for
    --   `ConT` - where this would happen.
    --
    --   TODO: Now i think this might need to be
    --   removed in favour of something smarter.

-- can we realise the type's kind to *?
canRealiseKindStar :: Type -> KindStatus
canRealiseKindStar = \case
  VarT{} -> KindStar
  SigT _ StarT -> KindStar
  SigT _ (VarT n) -> IsKindVar n
  ConT n -> IsCon n
  _ -> NotKindStar

-- discard the kind signature from a TyVarBndr.
tyVarBndrNoSig :: TyVarBndr -> TyVarBndr
tyVarBndrNoSig = \case
  PlainTV n -> PlainTV n
  KindedTV n _k -> PlainTV n

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
toMoatTypeECxt (unSigT -> typ) = AppE
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
  VarT n
    -> AppE (ConE 'Poly) (prettyTyVar n)
  SigT (VarT n) _
    -> AppE (ConE 'Poly) (prettyTyVar n)
  typ ->
    let decompressed = decompress typ
        prettyName = map Char.toUpper . TS.unpack . head . TS.splitOn "_" . last . TS.splitOn "." . TS.pack . show
        filledInHoles = decompressed <&>
          (\case
            VarT name -> AppT
              (ConT ''Moat.SingSymbol)
              (LitT (StrTyLit (prettyName name)))
            SigT (VarT name) _ -> AppT
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
  deriving stock (Functor,Foldable,Traversable)

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
aliasExp :: ()
  => Name
     -- ^ alias name
  -> [Type]
     -- ^ type variables
  -> Type
     -- ^ type (RHS)
  -> Exp
aliasExp name tyVars field = RecConE 'MoatAlias
  [ (mkName "aliasName", unqualName name)
  , (mkName "aliasTyVars", prettyTyVars tyVars)
  , (mkName "aliasTyp", toMoatTypeECxt field)
  ]

-- | Construct a Tag.
tagExp :: ()
  => Name
     -- ^ tycon name
  -> Name
     -- ^ parent name
  -> Type
     -- ^ type of the tag (RHS)
  -> Bool
     -- ^ Whether or not we are disambiguating.
  -> Exp
tagExp tyconName parentName typ dis = RecConE 'Tag
  [ (mkName "tagName", unqualName tyconName)
  , (mkName "tagParent", unqualName parentName)
  , (mkName "tagTyp", toMoatTypeECxt typ)
  , (mkName "tagDisambiguate", case dis of
      { False -> ConE 'False
      ; True  -> ConE 'True
      })
  ]

-- | Construct an Enum.
enumExp :: ()
  => Name
     -- ^ parent name
  -> [Type]
     -- ^ type variables
  -> [Interface]
     -- ^ interfaces
  -> [Protocol]
     -- ^ protocols
  -> [Exp]
     -- ^ cases
  -> Maybe MoatType
     -- ^ Raw Value
  -> [Exp]
     -- ^ Tags
  -> (Bool, Maybe MoatType, [Protocol])
     -- ^ Make base?
  -> Exp
enumExp parentName tyVars ifaces protos cases raw tags bs
  = applyBase bs $ RecConE 'MoatEnum
      [ (mkName "enumName", unqualName parentName)
      , (mkName "enumTyVars", prettyTyVars tyVars)
      , (mkName "enumInterfaces", ifacesExp ifaces)
      , (mkName "enumProtocols", protosExp protos)
      , (mkName "enumCases", ListE cases)
      , (mkName "enumRawValue", rawValueE raw)
      , (mkName "enumPrivateTypes", ListE [])
      , (mkName "enumTags", ListE tags)
      ]

newtypeExp :: ()
  => Name
  -> [Type]
  -> [Interface]
  -> [Protocol]
  -> Exp
  -> Exp
newtypeExp name tyVars ifaces protos field
  = RecConE 'MoatNewtype
      [ (mkName "newtypeName", unqualName name)
      , (mkName "newtypeTyVars", prettyTyVars tyVars)
      , (mkName "newtypeField", field)
      , (mkName "newtypeInterfaces", ifacesExp ifaces)
      , (mkName "newtypeProtocols", protosExp protos)
      ]

-- | Construct a Struct.
structExp :: ()
  => Name
     -- ^ struct name
  -> [Type]
     -- ^ type variables
  -> [Interface]
     -- ^ interfaces
  -> [Protocol]
     -- ^ protocols
  -> [Exp]
     -- ^ fields
  -> [Exp]
     -- ^ tags
  -> (Bool, Maybe MoatType, [Protocol])
     -- ^ Make base?
  -> Exp
structExp name tyVars ifaces protos fields tags bs
  = applyBase bs $ RecConE 'MoatStruct
      [ (mkName "structName", unqualName name)
      , (mkName "structTyVars", prettyTyVars tyVars)
      , (mkName "structInterfaces", ifacesExp ifaces)
      , (mkName "structProtocols", protosExp protos)
      , (mkName "structFields", ListE fields)
      , (mkName "structPrivateTypes", ListE [])
      , (mkName "structTags", ListE tags)
      ]

matchProxy :: Exp -> ShwiftyM Match
matchProxy e = lift $ match
  (conP 'Proxy [])
  (normalB (pure e))
  []

stripFields :: MoatData -> MoatData
stripFields = \case
  s@MoatStruct{} -> s { structFields = [] }
  s@MoatEnum{} -> s { enumCases = go (enumCases s) }
    where
      go = map stripOne
      stripOne (x, _) = (x, [])
  s -> s

giveProtos :: [Protocol] -> MoatData -> MoatData
giveProtos ps = \case
  s@MoatStruct{} -> s { structProtocols = ps }
  s@MoatEnum{} -> s { enumProtocols = ps }
  s -> s

suffixBase :: MoatData -> MoatData
suffixBase = \case
  s@MoatStruct{} -> s { structName = structName s ++ "Base" }
  s@MoatEnum{} -> s { enumName = enumName s ++ "Base" }
  s -> s

giveBase :: Maybe MoatType -> [Protocol] -> MoatData -> MoatData
giveBase r ps = \case
  s@MoatStruct{} -> s { structPrivateTypes = [giveProtos ps (suffixBase (stripFields s))] }
  s@MoatEnum{} -> s { enumPrivateTypes = [ giveProtos ps (suffixBase (stripFields s)) { enumRawValue = r }] }
  s -> s

-- | Apply 'giveBase' to a 'MoatData'.
--
--   Ideally we would offload this into
--   the first construction of the MoatData,
--   inside structExp/enumExp.
--
--
-- should we strip tyvars as well?
applyBase :: (Bool, Maybe MoatType, [Protocol]) -> Exp -> Exp
applyBase (b, r, ps) (ParensE -> s) = if b
  then
    AppE (AppE (AppE (VarE 'giveBase) (rawValueE r)) (protosExp ps)) s
  else s

-- TODO DONT USE SHOW
protosExp :: [Protocol] -> Exp
protosExp = ListE . map (ConE . mkName . go)
  where
    go = \case
      Equatable -> "Equatable"
      Hashable -> "Hashable"
      Codable -> "Codable"
      OtherProtocol p -> p

-- TODO DONT USE SHOW
ifacesExp :: [Interface] -> Exp
ifacesExp = ListE . map (ConE . mkName . go)
  where
    go = \case
      Serializable -> "Serializable"

tupE :: [Exp] -> Exp
#if MIN_VERSION_template_haskell(2,16,0)
tupE = TupE . map Just
#else
tupE = TupE
#endif
