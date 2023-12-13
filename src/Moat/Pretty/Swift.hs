module Moat.Pretty.Swift
  ( prettySwiftData
  , prettyMoatType
  )
where

import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Moat.Pretty.Doc.DocC
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
    prettyTypeDoc "" enumDoc []
      ++ "enum "
      ++ prettyMoatTypeHeader enumName enumTyVars
      ++ prettyRawValueAndProtocols enumRawValue enumProtocols
      ++ " {"
      ++ newlineNonEmpty enumCases
      ++ prettyEnumCases indents enumCases
      ++ newlineNonEmpty enumPrivateTypes
      ++ prettyPrivateTypes indents enumPrivateTypes
      ++ prettyTags indents enumTags
      ++ newlineNonEmpty enumTags
      ++ "}"
  MoatStruct {..} ->
    prettyTypeDoc "" structDoc []
      ++ "struct "
      ++ prettyMoatTypeHeader structName structTyVars
      ++ prettyRawValueAndProtocols Nothing structProtocols
      ++ " {"
      ++ newlineNonEmpty structFields
      ++ prettyStructFields indents structFields
      ++ newlineNonEmpty structPrivateTypes
      ++ prettyPrivateTypes indents structPrivateTypes
      ++ prettyTags indents structTags
      ++ newlineNonEmpty structTags
      ++ "}"
  MoatAlias {..} ->
    prettyTypeDoc "" aliasDoc []
      ++ "typealias "
      ++ prettyMoatTypeHeader aliasName aliasTyVars
      ++ " = "
      ++ prettyMoatType aliasTyp
  MoatNewtype {..} ->
    prettyTypeDoc "" newtypeDoc []
      ++ "struct "
      ++ prettyMoatTypeHeader newtypeName newtypeTyVars
      ++ prettyRawValueAndProtocols Nothing newtypeProtocols
      ++ " {\n"
      ++ indents
      ++ if isConcrete newtypeField
        then
          "let "
            ++ fieldName newtypeField
            ++ ": "
            ++ prettyMoatType (fieldType newtypeField)
            ++ "\n}"
        else
          "typealias "
            ++ newtypeName
            ++ "Tag"
            ++ " = Tagged<"
            ++ newtypeName
            ++ ", "
            ++ prettyMoatType (fieldType newtypeField)
            ++ ">\n"
            ++ prettyNewtypeField indents newtypeField newtypeName
            ++ "}"
  where
    indents = replicate indent ' '

    newlineNonEmpty [] = ""
    newlineNonEmpty _ = "\n"

    isConcrete :: Field -> Bool
    isConcrete = \case
      (Field _ Concrete {} _) -> True
      _ -> False

prettyTypeDoc :: String -> Maybe String -> [Field] -> String
prettyTypeDoc indents doc fields =
  let wrap = 100 - length indents - 4 -- "/// " doc comment prefix
      docC = intercalate "\n" (catMaybes [prettyDoc wrap <$> doc, prettyFieldDoc wrap fields])
   in prettyDocComment indents docC

prettyMoatTypeHeader :: String -> [String] -> String
prettyMoatTypeHeader name [] = name
prettyMoatTypeHeader name tyVars = name ++ "<" ++ intercalate ", " tyVars ++ ">"

prettyRawValueAndProtocols :: Maybe MoatType -> [Protocol] -> String
prettyRawValueAndProtocols Nothing [] = ""
prettyRawValueAndProtocols Nothing ps = ": " ++ prettyProtocols ps
prettyRawValueAndProtocols (Just ty) [] = ": " ++ prettyMoatType ty
prettyRawValueAndProtocols (Just ty) ps = ": " ++ prettyMoatType ty ++ ", " ++ prettyProtocols ps

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
prettyTags :: String -> [MoatType] -> String
prettyTags indents = go
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
        ++ prettyMoatType tagTyp
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

labelCase :: Field -> String
labelCase (Field "" ty _) = prettyMoatType ty
labelCase (Field label ty _) = "_ " ++ label ++ ": " ++ prettyMoatType ty

-- | Pretty-print a 'Ty'.
prettyMoatType :: MoatType -> String
prettyMoatType = \case
  Str -> "String"
  Unit -> "()"
  Bool -> "Bool"
  Character -> "Character"
  Tuple2 e1 e2 -> "(" ++ prettyMoatType e1 ++ ", " ++ prettyMoatType e2 ++ ")"
  Tuple3 e1 e2 e3 -> "(" ++ prettyMoatType e1 ++ ", " ++ prettyMoatType e2 ++ ", " ++ prettyMoatType e3 ++ ")"
  Optional o@(Optional _) -> prettyMoatType o
  Optional e -> prettyMoatType e ++ "?"
  -- Swift flips the parameters for Result, see https://developer.apple.com/documentation/swift/result
  Result e1 e2 -> "Result<" ++ prettyMoatType e2 ++ ", " ++ prettyMoatType e1 ++ ">"
  Set e -> "Set<" ++ prettyMoatType e ++ ">"
  Dictionary e1 e2 -> "Dictionary<" ++ prettyMoatType e1 ++ ", " ++ prettyMoatType e2 ++ ">"
  Array e -> "[" ++ prettyMoatType e ++ "]"
  -- App is special, we recurse until we no longer
  -- any applications.
  App e1 e2 -> prettyApp e1 e2
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
      ++ intercalate ", " (map prettyMoatType tys)
      ++ ">"
  Tag {..} -> tagParent ++ "." ++ tagName

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

prettyEnumCases :: String -> [EnumCase] -> String
prettyEnumCases indents = go
  where
    go = \case
      [] -> ""
      (EnumCase caseNm caseDoc [] : xs) ->
        prettyTypeDoc indents caseDoc []
          ++ indents
          ++ "case "
          ++ caseNm
          ++ "\n"
          ++ go xs
      (EnumCase caseNm caseDoc cs : xs) ->
        prettyTypeDoc indents caseDoc cs
          ++ indents
          ++ "case "
          ++ caseNm
          ++ "("
          ++ intercalate ", " (map labelCase cs)
          ++ ")\n"
          ++ go xs

prettyStructFields :: String -> [Field] -> String
prettyStructFields indents = go
  where
    go [] = ""
    go (Field {..} : fs) =
      prettyTypeDoc indents fieldDoc []
        ++ indents
        ++ "var "
        ++ fieldName
        ++ ": "
        ++ prettyMoatType fieldType
        ++ "\n"
        ++ go fs

prettyNewtypeField :: String -> Field -> String -> String
prettyNewtypeField indents (Field alias _ _) fieldName = indents ++ "let " ++ alias ++ ": " ++ fieldName ++ "Tag" ++ "\n"

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
