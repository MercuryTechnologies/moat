module Moat.Pretty.Kotlin
  ( prettyKotlinData,
  )
where

import qualified Data.Char as Char
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (catMaybes, mapMaybe)
import Moat.Pretty.Doc.KDoc
import Moat.Types

-- | Convert a 'MoatData' into a canonical representation in Kotlin
--
-- This is a decent default if you plan to do Android development, however you
-- could instead use this as a template to write your own version. Or, use it
-- to write an entirely new language backend :)
prettyKotlinData :: MoatData -> String
prettyKotlinData = \case
  MoatStruct {..} ->
    prettyStruct
      structName
      structDoc
      structTyVars
      structInterfaces
      structAnnotations
      structFields
      indents
  MoatEnum {..} ->
    prettyEnum
      enumDoc
      enumAnnotations
      enumInterfaces
      enumName
      enumTyVars
      enumCases
      enumSumOfProductEncodingOption
      enumEnumEncodingStyle
      indents
  MoatNewtype {..} ->
    prettyTypeDoc noIndent newtypeDoc [newtypeField]
      ++ prettyAnnotations Nothing noIndent newtypeAnnotations
      ++ "value class "
      ++ prettyMoatTypeHeader newtypeName (addTyVarBounds newtypeTyVars newtypeInterfaces)
      ++ "(val "
      ++ fieldName newtypeField
      ++ ": "
      ++ prettyMoatType (fieldType newtypeField)
      ++ ")"
      ++ prettyInterfaces newtypeInterfaces
  MoatAlias {..} ->
    prettyTypeDoc noIndent aliasDoc []
      ++ "typealias "
      ++ prettyMoatTypeHeader aliasName aliasTyVars
      ++ " = "
      ++ prettyMoatType aliasTyp
  where
    indent = 4
    indents = replicate indent ' '

prettyTypeDoc :: String -> Maybe String -> [Field] -> String
prettyTypeDoc indents doc fields =
  let wrap = wrapColumn indents 100
      kdoc = intercalate "\n" (catMaybes [prettyDoc wrap <$> doc, prettyFieldDoc wrap fields])
   in prettyDocComment wrap indents kdoc

prettyStructFields :: String -> [Field] -> String
prettyStructFields indents = go
  where
    go [] = ""
    go (Field fieldName ty _ : fs) =
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

prettyEnumCases :: String -> [EnumCase] -> String
prettyEnumCases indents = go
  where
    go = \case
      [] -> ""
      (EnumCase {..} : cases) ->
        prettyTypeDoc indents enumCaseDoc enumCaseFields
          ++ indents
          ++ enumCaseName
          ++ ",\n"
          ++ go cases

prettyValueClassInstances :: String -> String -> String -> [EnumCase] -> String
prettyValueClassInstances indents typ cons enumCases = case enumCases of
  [] -> ""
  _ ->
    indents
      ++ "companion object {\n"
      ++ instanceFields enumCases
      ++ "\n"
      ++ entriesField (map enumCaseName enumCases)
      ++ indents
      ++ "}"
  where
    entriesField :: [String] -> String
    entriesField [] = indents ++ indents ++ "val entries: List<" ++ typ ++ "> = emptyList()"
    entriesField caseNames =
      indents
        ++ indents
        ++ "val entries: List<"
        ++ typ
        ++ "> = listOf(\n"
        ++ concat (replicate 3 indents)
        ++ intercalate (concat (replicate 3 indents)) (map ((++ ",\n") . toUpperFirst) caseNames)
        ++ indents
        ++ indents
        ++ ")\n"

    instanceFields :: [EnumCase] -> String
    instanceFields [] = ""
    instanceFields (EnumCase {..} : cases) =
      prettyTypeDoc (indents ++ indents) enumCaseDoc []
        ++ indents
        ++ indents
        ++ "val "
        ++ toUpperFirst enumCaseName
        ++ ": "
        ++ typ
        ++ " = "
        ++ cons
        ++ "(\""
        ++ enumCaseName
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
  Optional o@(Optional _) -> prettyMoatType o
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
  [String] ->
  [Annotation] ->
  [Interface] ->
  [EnumCase] ->
  String ->
  SumOfProductEncodingOptions ->
  String
prettyTaggedObject parentName tyVars anns ifaces cases indents SumOfProductEncodingOptions {..} =
  intercalate
    "\n\n"
    ( cases <&> \case
        EnumCase caseNm caseDoc fields@[Field _ caseTy _] ->
          prettyTypeDoc indents caseDoc fields
            ++ prettyAnnotations (Just caseNm) indents anns
            ++ indents
            ++ "data class "
            ++ caseTypeHeader caseNm
            ++ "(val "
            ++ contentsFieldName
            ++ ": "
            ++ prettyMoatType caseTy
            ++ ") : "
            ++ parentTypeHeader
        EnumCase caseNm caseDoc [] ->
          prettyTypeDoc indents caseDoc []
            ++ prettyAnnotations (Just caseNm) indents anns
            ++ indents
            ++ "data object "
            ++ objectCaseTypeHeader caseNm
            ++ " : "
            ++ objectParentTypeHeader
        EnumCase caseNm _ _ ->
          error $
            "prettyTaggedObject: The data constructor "
              <> caseNm
              <> " can have zero or one concrete type constructor!"
    )
  where
    caseTypeHeader :: String -> String
    caseTypeHeader name = prettyMoatTypeHeader (toUpperFirst name) (addTyVarBounds tyVars ifaces)

    parentTypeHeader :: String
    parentTypeHeader = prettyMoatTypeHeader parentName tyVars

    objectCaseTypeHeader :: String -> String
    objectCaseTypeHeader name = prettyMoatTypeHeader (toUpperFirst name) []

    objectParentTypeHeader :: String
    objectParentTypeHeader = prettyMoatTypeHeader parentName (replicate (length tyVars) "Nothing")

prettyStruct ::
  () =>
  -- | name
  String ->
  -- | doc
  Maybe String ->
  -- | ty vars
  [String] ->
  -- | interfaces
  [Interface] ->
  -- | annotations
  [Annotation] ->
  -- | fields
  [Field] ->
  -- | indents
  String ->
  String
prettyStruct name doc tyVars ifaces anns fields indents =
  prettyTypeDoc noIndent doc fields
    ++ prettyAnnotations Nothing noIndent anns
    ++ body
    ++ prettyInterfaces ifaces
  where
    body :: String
    body =
      case fields of
        [] -> case tyVars of
          [] -> "data object " ++ prettyMoatTypeHeader name []
          _ -> "class " ++ prettyMoatTypeHeader name (addTyVarBounds tyVars ifaces)
        _ ->
          "data class "
            ++ prettyMoatTypeHeader name (addTyVarBounds tyVars ifaces)
            ++ "(\n"
            ++ prettyStructFields indents fields
            ++ ")"

prettyEnum ::
  () =>
  -- | doc
  Maybe String ->
  [Annotation] ->
  -- | interfaces
  [Interface] ->
  -- | name
  String ->
  -- | ty vars
  [String] ->
  -- | cases
  [EnumCase] ->
  -- | encoding style
  SumOfProductEncodingOptions ->
  -- | enum style
  EnumEncodingStyle ->
  -- | indents
  String ->
  String
prettyEnum doc anns ifaces name tyVars cases sop@SumOfProductEncodingOptions {..} ees indents
  | isCEnum cases =
      case ees of
        EnumClassStyle ->
          prettyTypeDoc noIndent doc []
            ++ prettyAnnotations Nothing noIndent (dontAddSerializeToEnums anns)
            ++ "enum class "
            ++ classTyp
            ++ prettyInterfaces ifaces
            ++ " {"
            ++ newlineNonEmpty cases
            ++ prettyEnumCases indents cases
            ++ "}"
        ValueClassStyle ->
          prettyTypeDoc noIndent doc []
            ++ prettyAnnotations Nothing noIndent (ensureJvmInlineForValueClasses anns)
            ++ "value class "
            ++ classTyp
            ++ "(val value: String)"
            ++ prettyInterfaces ifaces
            ++ " {"
            ++ newlineNonEmpty cases
            ++ prettyValueClassInstances indents classTyp name cases
            ++ newlineNonEmpty cases
            ++ "}"
  | otherwise =
      case encodingStyle of
        TaggedFlatObjectStyle ->
          prettyTypeDoc noIndent doc []
            ++ prettyAnnotations Nothing noIndent (dontAddParcelizeToSealedClasses anns)
            ++ "sealed interface "
            ++ classTyp
            ++ prettyInterfaces ifaces
        TaggedObjectStyle ->
          prettyTypeDoc noIndent doc []
            ++ prettyAnnotations
              Nothing
              noIndent
              (dontAddParcelizeToSealedClasses (sumAnnotations ++ anns))
            ++ "sealed interface "
            ++ classTyp
            ++ prettyInterfaces ifaces
            ++ " {\n"
            ++ prettyTaggedObject name tyVars anns ifaces cases indents sop
            ++ "\n}"
  where
    isCEnum :: [EnumCase] -> Bool
    isCEnum = all ((== []) . enumCaseFields)

    classTyp :: String
    classTyp = prettyMoatTypeHeader name (addTyVarBounds tyVars ifaces)

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

addTyVarBounds :: [String] -> [Interface] -> [String]
addTyVarBounds tyVars ifaces
  | Parcelable `elem` ifaces = map (++ " : Parcelable") tyVars
  | otherwise = tyVars

wrapColumn :: String -> Int -> Int
wrapColumn indents col = col - length indents - 3 -- " * " doc comment prefix
