{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Moat.Pretty.Kotlin
  ( prettyKotlinData,
  )
where

import qualified Data.Char as Char
import Data.List (intercalate)
import Moat.Types

prettyKotlinData :: MoatData -> String
prettyKotlinData = \case
  MoatStruct {..} ->
    ""
      ++ prettyAnnotations structAnnotations
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
  MoatNewtype {..} ->
    ""
      ++ prettyAnnotations newtypeAnnotations
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
    go ((fieldName, ty) : fs) = indents ++ "val " ++ fieldName ++ ": " ++ prettyMoatType ty ++ ",\n" ++ go fs

prettyCEnumCases :: String -> [String] -> String
prettyCEnumCases indents = go
  where
    go = \case
      [] -> ""
      (caseName : cases) ->
        []
          ++ indents
          ++ caseName
          ++ ",\n"
          ++ go cases

prettyEnumCases :: String -> String -> [(String, [(Maybe String, MoatType)])] -> String
prettyEnumCases typName indents = go
  where
    go = \case
      [] -> ""
      ((caseNm, []) : xs) ->
        []
          ++ indents
          ++ "object "
          ++ toUpperFirst caseNm
          ++ "() : "
          ++ typName
          ++ "\n"
          ++ go xs
      ((caseNm, cs) : xs) ->
        []
          ++ indents
          ++ "data class "
          ++ toUpperFirst caseNm
          ++ "(\n"
          ++ intercalate
            ",\n"
            ( map
                ((++) indents)
                ( (map ((++) indents . uncurry labelCase) cs)
                )
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

prettyAnnotations :: [Annotation] -> String
prettyAnnotations = concatMap (\ann -> "@" ++ prettyAnnotation ann ++ "\n")
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
  String
prettyEnum anns ifaces name tyVars cases indents
  | isCEnum cases =
    prettyAnnotations (dontAddSerializeToEnums anns)
      ++ "enum class "
      ++ prettyMoatTypeHeader name tyVars
      ++ prettyInterfaces ifaces
      ++ " {"
      ++ newlineNonEmpty cases
      ++ prettyCEnumCases indents (map fst cases)
      ++ "}"
  | allConcrete cases =
    prettyAnnotations anns
      ++ "sealed class "
      ++ prettyMoatTypeHeader name tyVars
      ++ prettyInterfaces ifaces
  | otherwise =
    prettyAnnotations (dontAddSerializeToEnums anns)
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
        moatTypes = fmap snd . concat $ fmap snd inp
        isConcrete (Concrete {}) = True
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
