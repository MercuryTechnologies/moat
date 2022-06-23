module Moat.Pretty.Kotlin
  ( prettyKotlinData,
  )
where

import qualified Data.Char as Char
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Moat.Types

-- | Convert a 'MoatData' into a canonical representation in Kotlin
--
-- This is a decent default if you plan to do Android development, however you
-- could instead use this as a template to write your own version. Or, use it
-- to write an entirely new language backend :)
prettyKotlinData :: MoatData -> String
prettyKotlinData = \case
  MoatStruct {..} ->
    ""
      ++ prettyAnnotations Nothing noIndent structAnnotations
      ++ "data class "
      ++ prettyMoatTypeHeader structName structTyVars
      ++ "("
      ++ newlineNonEmpty structFields
      ++ prettyStructFields indents structFields
      ++ ")"
      ++ prettyInterfaces structInterfaces
  MoatEnum {..} ->
    prettyEnum
      enumAnnotations
      enumInterfaces
      enumName
      enumTyVars
      enumCases
      enumSumOfProductEncodingOption
      enumEnumEncodingStyle
      indents
  MoatNewtype {..} ->
    ""
      ++ prettyAnnotations Nothing noIndent newtypeAnnotations
      ++ "value class "
      ++ prettyMoatTypeHeader newtypeName newtypeTyVars
      ++ "(val "
      ++ fst newtypeField
      ++ ": "
      ++ prettyMoatType (snd newtypeField)
      ++ ")"
      ++ prettyInterfaces newtypeInterfaces
  MoatAlias {..} ->
    ""
      ++ "typealias "
      ++ prettyMoatTypeHeader aliasName aliasTyVars
      ++ " = "
      ++ prettyMoatType aliasTyp
  where
    indent = 4
    indents = replicate indent ' '

prettyStructFields :: String -> [(String, MoatType)] -> String
prettyStructFields indents = go
  where
    go [] = ""
    go ((fieldName, ty) : fs) =
      indents
        ++ "val "
        ++ fieldName
        ++ ": "
        ++ prettyMoatType ty
        ++ case ty of
          Optional _ -> " = null"
          _ -> ""
        ++ ",\n"
        ++ go fs

prettyEnumCases :: String -> [String] -> String
prettyEnumCases indents = go
  where
    go = \case
      [] -> ""
      (caseName : cases) ->
        indents
          ++ caseName
          ++ ",\n"
          ++ go cases

prettyValueClassInstances :: String -> String -> [String] -> String
prettyValueClassInstances indents cons enumCases = case enumCases of
  [] -> ""
  _ ->
    indents
      ++ "companion object {\n"
      ++ instanceFields enumCases
      ++ "\n"
      ++ entriesField enumCases
      ++ indents
      ++ "}"
  where
    entriesField :: [String] -> String
    entriesField [] = indents ++ indents ++ "val entries = emptyList()"
    entriesField _ =
      indents
        ++ indents
        ++ "val entries = listOf("
        ++ intercalate ", " (map toUpperFirst enumCases)
        ++ ")\n"

    instanceFields :: [String] -> String
    instanceFields [] = ""
    instanceFields (caseName : cases) =
      indents
        ++ indents
        ++ "val "
        ++ toUpperFirst caseName
        ++ " = "
        ++ cons
        ++ "(\""
        ++ caseName
        ++ "\")\n"
        ++ instanceFields cases

prettyMoatTypeHeader :: String -> [String] -> String
prettyMoatTypeHeader name [] = name
prettyMoatTypeHeader name tyVars = name ++ "<" ++ intercalate ", " tyVars ++ ">"

-- | This function will take a name and the indentation level and render
-- annotations in the style '@{string}\n...'. The name parameter is only used
-- when a 'SerialName' annotation is given for a sum of product
prettyAnnotations :: Maybe String -> String -> [Annotation] -> String
prettyAnnotations mCaseNm indents =
  concatMap (\ann -> indents <> "@" <> ann <> "\n")
    . mapMaybe prettyAnnotation
  where
    prettyAnnotation :: Annotation -> Maybe String
    prettyAnnotation = \case
      JvmInline -> Just "JvmInline"
      Parcelize -> Just "Parcelize"
      Serializable -> Just "Serializable"
      SerialName -> mCaseNm <&> \caseNm -> "SerialName(\"" <> caseNm <> "\")"
      RawAnnotation s -> Just s

prettyInterfaces :: [Interface] -> String
prettyInterfaces [] = ""
prettyInterfaces ps = " : " ++ intercalate ", " (prettyInterface <$> ps)
  where
    prettyInterface :: Interface -> String
    prettyInterface = \case
      Parcelable -> "Parcelable"
      RawInterface s -> s
      LinkEnumInterface s -> s ++ "()"

-- | Pretty-print a 'Ty'.
prettyMoatType :: MoatType -> String
prettyMoatType = \case
  Str -> "String"
  Unit -> "()"
  Bool -> "Boolean"
  Character -> "Char"
  Tuple2 e1 e2 -> "(" ++ prettyMoatType e1 ++ ", " ++ prettyMoatType e2 ++ ")"
  Tuple3 e1 e2 e3 -> "(" ++ prettyMoatType e1 ++ ", " ++ prettyMoatType e2 ++ ", " ++ prettyMoatType e3 ++ ")"
  Optional e -> prettyMoatType e ++ "?"
  Result e1 e2 -> "Either<" ++ prettyMoatType e1 ++ ", " ++ prettyMoatType e2 ++ ">"
  Set e -> "Set<" ++ prettyMoatType e ++ ">"
  Dictionary e1 e2 -> "Map<" ++ prettyMoatType e1 ++ ", " ++ prettyMoatType e2 ++ ">"
  Array e -> "List<" ++ prettyMoatType e ++ ">"
  -- App is special, we recurse until we no longer
  -- any applications.
  App e1 e2 -> prettyApp e1 e2
  I -> "Int"
  I8 -> "Byte"
  I16 -> "Short"
  I32 -> "Int"
  I64 -> "Long"
  U -> "UInt"
  U8 -> "UByte"
  U16 -> "UShort"
  U32 -> "UInt"
  U64 -> "ULong"
  F32 -> "Float"
  F64 -> "Double"
  Decimal -> "Decimal"
  BigInt -> "BigInteger"
  Poly ty -> ty
  Concrete ty [] -> ty
  Concrete ty tys ->
    ty
      ++ "<"
      ++ intercalate ", " (map prettyMoatType tys)
      ++ ">"
  Tag {..} -> tagName

prettyApp :: MoatType -> MoatType -> String
prettyApp t1 t2 =
  "(("
    ++ intercalate ", " (map prettyMoatType as)
    ++ ") -> "
    ++ prettyMoatType r
    ++ ")"
  where
    (as, r) = go t1 t2
    go e1 (App e2 e3) = case go e2 e3 of
      (args, ret) -> (e1 : args, ret)
    go e1 e2 = ([e1], e2)

{- HLINT ignore prettyTaggedObject "Avoid restricted function" -}
-- error is restricted
prettyTaggedObject ::
  String ->
  [Annotation] ->
  [(String, [(Maybe String, MoatType)])] ->
  String ->
  SumOfProductEncodingOptions ->
  String
prettyTaggedObject parentName anns cases indents SumOfProductEncodingOptions {..} =
  intercalate
    "\n\n"
    ( cases <&> \case
        (caseNm, [(_, caseTy)]) ->
          prettyAnnotations (Just caseNm) indents anns
            ++ indents
            ++ "data class "
            ++ toUpperFirst caseNm
            ++ "(val "
            ++ contentsFieldName
            ++ ": "
            ++ prettyMoatType caseTy
            ++ ") : "
            ++ parentName
            ++ "()"
        (caseNm, []) ->
          prettyAnnotations (Just caseNm) indents anns
            ++ indents
            ++ "object "
            ++ toUpperFirst caseNm
            ++ " : "
            ++ parentName
            ++ "()"
        (caseNm, _) ->
          error $
            "prettyTaggedObject: The data constructor "
              <> caseNm
              <> " can have zero or one concrete type constructor!"
    )

prettyEnum ::
  () =>
  [Annotation] ->
  -- | interfaces
  [Interface] ->
  -- | name
  String ->
  -- | ty vars
  [String] ->
  -- | cases
  [(String, [(Maybe String, MoatType)])] ->
  -- | encoding style
  SumOfProductEncodingOptions ->
  -- | enum style
  EnumEncodingStyle ->
  -- | indents
  String ->
  String
prettyEnum anns ifaces name tyVars cases sop@SumOfProductEncodingOptions {..} ees indents
  | isCEnum cases =
      case ees of
        EnumClassStyle ->
          prettyAnnotations Nothing noIndent (dontAddSerializeToEnums anns)
            ++ "enum class "
            ++ prettyMoatTypeHeader name tyVars
            ++ prettyInterfaces ifaces
            ++ " {"
            ++ newlineNonEmpty cases
            ++ prettyEnumCases indents (map fst cases)
            ++ "}"
        ValueClassStyle ->
          prettyAnnotations Nothing noIndent (ensureJvmInlineForValueClasses anns)
            ++ "value class "
            ++ prettyMoatTypeHeader name tyVars
            ++ "(val value: String)"
            ++ prettyInterfaces ifaces
            ++ " {"
            ++ newlineNonEmpty cases
            ++ prettyValueClassInstances indents name (map fst cases)
            ++ newlineNonEmpty cases
            ++ "}"
  | otherwise =
      case encodingStyle of
        TaggedFlatObjectStyle ->
          prettyAnnotations Nothing noIndent (dontAddParcelizeToSealedClasses anns)
            ++ "sealed class "
            ++ prettyMoatTypeHeader name tyVars
            ++ prettyInterfaces ifaces
        TaggedObjectStyle ->
          prettyAnnotations
            Nothing
            noIndent
            (dontAddParcelizeToSealedClasses (sumAnnotations ++ anns))
            ++ "sealed class "
            ++ prettyMoatTypeHeader name tyVars
            ++ prettyInterfaces ifaces
            ++ " {\n"
            ++ prettyTaggedObject name anns cases indents sop
            ++ "\n}"
  where
    isCEnum :: Eq b => [(a, [b])] -> Bool
    isCEnum = all ((== []) . snd)

    -- because they get it automatically
    dontAddSerializeToEnums :: [Annotation] -> [Annotation]
    dontAddSerializeToEnums = filter (/= Serializable)

    -- because Parcelize should only be applied to concrete implementations
    dontAddParcelizeToSealedClasses :: [Annotation] -> [Annotation]
    dontAddParcelizeToSealedClasses = filter (/= Parcelize)

    ensureJvmInlineForValueClasses :: [Annotation] -> [Annotation]
    ensureJvmInlineForValueClasses as
      | JvmInline `elem` as = as
      | otherwise = as ++ [JvmInline]

newlineNonEmpty :: [a] -> String
newlineNonEmpty [] = ""
newlineNonEmpty _ = "\n"

toUpperFirst :: String -> String
toUpperFirst = \case
  [] -> []
  (c : cs) -> Char.toUpper c : cs

noIndent :: String
noIndent = ""
