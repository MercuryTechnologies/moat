module Moat.Pretty.Swift
  ( prettySwiftData,
  )
where

import Data.List (intercalate)
import Moat.Types

-- | Convert a 'MoatData' into a canonical representation in Swift
--
-- This is a decent default if you plan to do iOS development, however you
-- could instead use this as a template to write your own version. Or, use it
-- to write an entirely new language backend :)
prettySwiftData :: MoatData -> String
prettySwiftData = prettySwiftDataWith 4

-- | Pretty-print a 'SwiftData'.
--   This function cares about indent.
prettySwiftDataWith ::
  () =>
  -- | indent
  Int ->
  MoatData ->
  String
prettySwiftDataWith indent = \case
  MoatEnum {..} ->
    "enum "
      ++ prettyMoatTypeHeader enumName enumTyVars
      ++ prettyRawValueAndProtocols (OptionalExpand enumOptionalExpand) enumRawValue enumProtocols
      ++ " {"
      ++ newlineNonEmpty enumCases
      ++ prettyEnumCases (OptionalExpand enumOptionalExpand) indents enumCases
      ++ newlineNonEmpty enumPrivateTypes
      ++ prettyPrivateTypes indents enumPrivateTypes
      ++ prettyTags (OptionalExpand enumOptionalExpand) indents enumTags
      ++ newlineNonEmpty enumTags
      ++ "}"
  MoatStruct {..} ->
    "struct "
      ++ prettyMoatTypeHeader structName structTyVars
      ++ prettyRawValueAndProtocols (OptionalExpand structOptionalExpand) Nothing structProtocols
      ++ " {"
      ++ newlineNonEmpty structFields
      ++ prettyStructFields (OptionalExpand structOptionalExpand) indents structFields
      ++ newlineNonEmpty structPrivateTypes
      ++ prettyPrivateTypes indents structPrivateTypes
      ++ prettyTags (OptionalExpand structOptionalExpand) indents structTags
      ++ newlineNonEmpty structTags
      ++ "}"
  MoatAlias {..} ->
    "typealias "
      ++ prettyMoatTypeHeader aliasName aliasTyVars
      ++ " = "
      ++ prettyMoatType (OptionalExpand aliasOptionalExpand) aliasTyp
  MoatNewtype {..} ->
    "struct "
      ++ prettyMoatTypeHeader newtypeName newtypeTyVars
      ++ prettyRawValueAndProtocols (OptionalExpand newtypeOptionalExpand) Nothing newtypeProtocols
      ++ " {\n"
      ++ indents
      ++ if isConcrete newtypeField
        then
          "let "
            ++ fst newtypeField
            ++ ": "
            ++ prettyMoatType (OptionalExpand newtypeOptionalExpand) (snd newtypeField)
            ++ "\n}"
        else
          "typealias "
            ++ newtypeName
            ++ "Tag"
            ++ " = Tagged<"
            ++ newtypeName
            ++ ", "
            ++ prettyMoatType (OptionalExpand newtypeOptionalExpand) (snd newtypeField)
            ++ ">\n"
            ++ prettyNewtypeField indents newtypeField newtypeName
            ++ "}"
  where
    indents = replicate indent ' '

    newlineNonEmpty [] = ""
    newlineNonEmpty _ = "\n"

    isConcrete :: (a, MoatType) -> Bool
    isConcrete = \case
      (_, Concrete {}) -> True
      _ -> False

prettyMoatTypeHeader :: String -> [String] -> String
prettyMoatTypeHeader name [] = name
prettyMoatTypeHeader name tyVars = name ++ "<" ++ intercalate ", " tyVars ++ ">"

prettyRawValueAndProtocols :: OptionalExpand -> Maybe MoatType -> [Protocol] -> String
prettyRawValueAndProtocols _ Nothing [] = ""
prettyRawValueAndProtocols _ Nothing ps = ": " ++ prettyProtocols ps
prettyRawValueAndProtocols o (Just ty) [] = ": " ++ prettyMoatType o ty
prettyRawValueAndProtocols o (Just ty) ps = ": " ++ prettyMoatType o ty ++ ", " ++ prettyProtocols ps

prettyProtocols :: [Protocol] -> String
prettyProtocols = \case
  [] -> ""
  ps -> intercalate ", " (prettyProtocol <$> ps)
    where
      prettyProtocol :: Protocol -> String
      prettyProtocol = \case
        Hashable -> "Hashable"
        Codable -> "Codable"
        Equatable -> "Equatable"
        OtherProtocol s -> s

-- TODO: Need a plan to avoid @error@ in these pure functions
{-# ANN prettyTags "HLint: ignore" #-}
prettyTags :: OptionalExpand -> String -> [MoatType] -> String
prettyTags o indents = go
  where
    go [] = ""
    go (Tag {..} : ts) =
      "\n"
        ++ prettyTagDisambiguator tagDisambiguate indents tagName
        ++ indents
        ++ "typealias "
        ++ tagName
        ++ " = Tagged<"
        ++ (if tagDisambiguate then tagName ++ "Tag" else tagParent)
        ++ ", "
        ++ prettyMoatType o tagTyp
        ++ ">"
        ++ go ts
    go _ = error "non-tag supplied to prettyTags"

prettyTagDisambiguator ::
  () =>
  -- | disambiguate?
  Bool ->
  -- | indents
  String ->
  -- | parent type name
  String ->
  String
prettyTagDisambiguator disambiguate indents parent =
  if disambiguate
    then
      indents
        ++ "enum "
        ++ parent
        ++ "Tag { }\n"
    else ""

labelCase :: OptionalExpand -> Maybe String -> MoatType -> String
labelCase o Nothing ty = prettyMoatType o ty
labelCase o (Just label) ty = "_ " ++ label ++ ": " ++ prettyMoatType o ty

newtype OptionalExpand = OptionalExpand Bool

-- | Pretty-print a 'Ty'.
prettyMoatType :: OptionalExpand -> MoatType -> String
prettyMoatType o@(OptionalExpand oe) = \case
  Str -> "String"
  Unit -> "()"
  Bool -> "Bool"
  Character -> "Character"
  Tuple2 e1 e2 -> "(" ++ prettyMoatType o e1 ++ ", " ++ prettyMoatType o e2 ++ ")"
  Tuple3 e1 e2 e3 -> "(" ++ prettyMoatType o e1 ++ ", " ++ prettyMoatType o e2 ++ ", " ++ prettyMoatType o e3 ++ ")"
  Optional e ->
    if oe
      then "Optional<" <> prettyMoatType o e <> ">"
      else prettyMoatType o e <> "?"
  -- Swift flips the parameters for Result, see https://developer.apple.com/documentation/swift/result
  Result e1 e2 -> "Result<" ++ prettyMoatType o e2 ++ ", " ++ prettyMoatType o e1 ++ ">"
  Set e -> "Set<" ++ prettyMoatType o e ++ ">"
  Dictionary e1 e2 -> "Dictionary<" ++ prettyMoatType o e1 ++ ", " ++ prettyMoatType o e2 ++ ">"
  Array e -> "[" ++ prettyMoatType o e ++ "]"
  -- App is special, we recurse until we no longer
  -- any applications.
  App e1 e2 -> prettyApp o e1 e2
  I -> "Int"
  I8 -> "Int8"
  I16 -> "Int16"
  I32 -> "Int32"
  I64 -> "Int64"
  U -> "UInt"
  U8 -> "UInt8"
  U16 -> "UInt16"
  U32 -> "UInt32"
  U64 -> "UInt64"
  F32 -> "Float"
  F64 -> "Double"
  Decimal -> "Decimal"
  BigInt -> "BigInteger"
  Poly ty -> ty
  Concrete ty [] -> ty
  Concrete ty tys ->
    ty
      ++ "<"
      ++ intercalate ", " (map (prettyMoatType o) tys)
      ++ ">"
  Tag {..} -> tagParent ++ "." ++ tagName

prettyApp :: OptionalExpand -> MoatType -> MoatType -> String
prettyApp o t1 t2 =
  "(("
    ++ intercalate ", " (map (prettyMoatType o) as)
    ++ ") -> "
    ++ prettyMoatType o r
    ++ ")"
  where
    (as, r) = go t1 t2
    go e1 (App e2 e3) = case go e2 e3 of
      (args, ret) -> (e1 : args, ret)
    go e1 e2 = ([e1], e2)

prettyEnumCases :: OptionalExpand -> String -> [(String, [(Maybe String, MoatType)])] -> String
prettyEnumCases o indents = go
  where
    go = \case
      [] -> ""
      ((caseNm, []) : xs) ->
        indents
          ++ "case "
          ++ caseNm
          ++ "\n"
          ++ go xs
      ((caseNm, cs) : xs) ->
        indents
          ++ "case "
          ++ caseNm
          ++ "("
          ++ intercalate ", " (map (uncurry $ labelCase o) cs)
          ++ ")\n"
          ++ go xs

prettyStructFields :: OptionalExpand -> String -> [(String, MoatType)] -> String
prettyStructFields o indents = go
  where
    go [] = ""
    go ((fieldName, ty) : fs) = indents ++ "let " ++ fieldName ++ ": " ++ prettyMoatType o ty ++ "\n" ++ go fs

prettyNewtypeField :: String -> (String, MoatType) -> String -> String
prettyNewtypeField indents (alias, _) fieldName = indents ++ "let " ++ alias ++ ": " ++ fieldName ++ "Tag" ++ "\n"

prettyPrivateTypes :: String -> [MoatData] -> String
prettyPrivateTypes indents = go
  where
    go [] = ""
    go (s : ss) = indents ++ "private " ++ unlines (onLast (indents ++) (lines (prettySwiftData s))) ++ go ss

-- map a function over everything but the
-- first element.
onLast :: (a -> a) -> [a] -> [a]
onLast _ [] = []
onLast f (x : xs) = x : map f xs
