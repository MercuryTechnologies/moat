module Moat.Pretty.Kotlin.TaggedObject
  ( prettyKotlinData,
    TaggedObject (..),
  )
where

import qualified Data.Char as Char
import Data.Functor ((<&>))
import Data.List (intercalate)
import Moat.Types

data TaggedObject = TaggedObject
  { tagFieldName :: String,
    contentsFieldName :: String
  }

-- | Convert a 'MoatData' into a canonical representation in Kotlin
--
-- This is a decent default if you plan to do Android development, however you
-- could instead use this as a template to write your own version. Or, use it
-- to write an entirely new language backend :)
prettyKotlinData :: TaggedObject -> MoatData -> String
prettyKotlinData tags = \case
  MoatStruct {..} ->
    prettyAnnotations noIndent structAnnotations
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
      indents
      tags
  MoatNewtype {..} ->
    ""
      ++ prettyAnnotations indents newtypeAnnotations
      ++ "inline class "
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

prettyCEnumCases :: String -> [String] -> String
prettyCEnumCases indents = go
  where
    go = \case
      [] -> ""
      (caseName : cases) ->
        indents
          ++ caseName
          ++ ",\n"
          ++ go cases

prettyEnumCases :: String -> String -> [(String, [(Maybe String, MoatType)])] -> String
prettyEnumCases typName indents = go
  where
    go = \case
      [] -> ""
      ((caseNm, []) : xs) ->
        indents
          ++ "object "
          ++ toUpperFirst caseNm
          ++ "() : "
          ++ typName
          ++ "\n"
          ++ go xs
      ((caseNm, cs) : xs) ->
        indents
          ++ "data class "
          ++ toUpperFirst caseNm
          ++ "(\n"
          ++ intercalate
            ",\n"
            ( map
                ( (indents ++)
                    . (++) indents
                    . uncurry labelCase
                )
                cs
            )
          ++ "\n"
          ++ indents
          ++ ")\n"
          ++ go xs

labelCase :: Maybe String -> MoatType -> String
labelCase Nothing ty = prettyMoatType ty
labelCase (Just label) ty = "val " ++ label ++ ": " ++ prettyMoatType ty

prettyMoatTypeHeader :: String -> [String] -> String
prettyMoatTypeHeader name [] = name
prettyMoatTypeHeader name tyVars = name ++ "<" ++ intercalate ", " tyVars ++ ">"

prettyAnnotations :: String -> [Annotation] -> String
prettyAnnotations indents = concatMap (\ann -> indents <> "@" <> prettyAnnotation ann <> "\n")
  where
    prettyAnnotation :: Annotation -> String
    prettyAnnotation = \case
      Parcelize -> "Parcelize"
      Serializable -> "Serializable"
      RawAnnotation s -> s

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

prettySumOfProduct_Sum :: String -> [Annotation] -> [(String, [(Maybe String, MoatType)])] -> String -> TaggedObject -> String
prettySumOfProduct_Sum parentName anns cases indents TaggedObject {..} =
  intercalate
    "\n\n"
    ( cases <&> \(caseNm, [(_, Concrete {concreteName = concreteName})]) ->
        prettyAnnotations indents (anns ++ [RawAnnotation $ "SerialName(\"" <> caseNm <> "\")"])
          ++ indents
          ++ "data class "
          ++ toUpperFirst caseNm
          ++ "(val "
          ++ contentsFieldName
          ++ ": "
          ++ concreteName
          ++ ") : "
          ++ parentName
          ++ "()"
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
  -- | indents
  String ->
  -- | tags
  TaggedObject ->
  String
prettyEnum anns ifaces name tyVars cases indents to@TaggedObject {..}
  | isCEnum cases =
    prettyAnnotations indents (dontAddSerializeToEnums anns)
      ++ "enum class "
      ++ prettyMoatTypeHeader name tyVars
      ++ prettyInterfaces ifaces
      ++ " {"
      ++ newlineNonEmpty cases
      ++ prettyCEnumCases indents (map fst cases)
      ++ "}"
  | allConcrete cases =
    prettyAnnotations
      noIndent
      [RawAnnotation ("JsonClassDiscriminator(\"" <> tagFieldName <> "\")")]
      ++ "sealed class "
      ++ prettyMoatTypeHeader name tyVars
      ++ prettyInterfaces ifaces
      ++ " {\n"
      ++ prettySumOfProduct_Sum name anns cases indents to
      ++ "\n}"
  | otherwise =
    prettyAnnotations indents (dontAddSerializeToEnums anns)
      ++ "enum class "
      ++ prettyMoatTypeHeader name tyVars
      ++ prettyInterfaces ifaces
      ++ " {"
      ++ newlineNonEmpty cases
      ++ prettyEnumCases name indents cases
      ++ "}"
  where
    isCEnum :: Eq b => [(a, [b])] -> Bool
    isCEnum = all ((== []) . snd)

    allConcrete :: [(a, [(b, MoatType)])] -> Bool
    allConcrete inp = all isConcrete moatTypes
      where
        moatTypes = fmap snd (concatMap snd inp)
        isConcrete Concrete {} = True
        isConcrete _ = False

    -- because they get it automatically
    dontAddSerializeToEnums :: [Annotation] -> [Annotation]
    dontAddSerializeToEnums = filter (/= Serializable)

newlineNonEmpty :: [a] -> String
newlineNonEmpty [] = ""
newlineNonEmpty _ = "\n"

toUpperFirst :: String -> String
toUpperFirst = \case
  [] -> []
  (c : cs) -> Char.toUpper c : cs

noIndent :: String
noIndent = ""
