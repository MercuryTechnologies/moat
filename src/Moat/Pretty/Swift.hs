{-# language
    LambdaCase
  , RecordWildCards
  #-}

module Moat.Pretty.Swift
  ( prettySwiftData
  ) where

import Data.List (intercalate)
import Moat.Types

-- | Pretty-print a 'SwiftData'.
prettySwiftData :: SwiftData -> String
prettySwiftData = prettySwiftDataWith 4

prettyKotlinDataWith :: ()
  => Int
  -> KotlinData
  -> String
prettyKotlinDataWith indent = \case
  _ -> ""

-- | Pretty-print a 'SwiftData'.
--   This function cares about indent.
prettySwiftDataWith :: ()
  => Int -- ^ indent
  -> SwiftData
  -> String
prettySwiftDataWith indent = \case

  MoatEnum {..} -> []
    ++ "enum "
    ++ prettyMoatTypeHeader enumName enumTyVars
    ++ prettyRawValueAndProtocols enumRawValue enumInterfaces
    ++ " {"
    ++ newlineNonEmpty enumCases
    ++ prettyEnumCases indents enumCases
    ++ newlineNonEmpty enumPrivateTypes
    ++ prettyPrivateTypes indents enumPrivateTypes
    ++ prettyTags indents enumTags
    ++ newlineNonEmpty enumTags
    ++ "}"

  MoatStruct {..} -> []
    ++ "struct "
    ++ prettyMoatTypeHeader structName structTyVars
    ++ prettyProtocols structInterfaces
    ++ " {"
    ++ newlineNonEmpty structFields
    ++ prettyStructFields indents structFields
    ++ newlineNonEmpty structPrivateTypes
    ++ prettyPrivateTypes indents structPrivateTypes
    ++ prettyTags indents structTags
    ++ newlineNonEmpty structTags
    ++ "}"

  MoatAlias{..} -> []
    ++ "typealias "
    ++ prettyMoatTypeHeader aliasName aliasTyVars
    ++ " = "
    ++ prettyMoatType aliasTyp
  where
    indents = replicate indent ' '

    newlineNonEmpty [] = ""
    newlineNonEmpty _ = "\n"

prettyMoatTypeHeader :: String -> [String] -> String
prettyMoatTypeHeader name [] = name
prettyMoatTypeHeader name tyVars = name ++ "<" ++ intercalate ", " tyVars ++ ">"

prettyRawValueAndProtocols :: Maybe MoatType -> [Protocol] -> String
prettyRawValueAndProtocols Nothing ps = prettyProtocols ps
prettyRawValueAndProtocols (Just ty) [] = ": " ++ prettyMoatType ty
prettyRawValueAndProtocols (Just ty) ps = ": " ++ prettyMoatType ty ++ ", " ++ intercalate ", " (map prettyProtocol ps)

prettyProtocol :: Protocol -> String
prettyProtocol = \case
  Equatable -> "Equatable"
  Hashable -> "Hashable"
  Codable -> "Codable"
  OtherProtocol p -> p

prettyProtocols :: [Protocol] -> String
prettyProtocols = \case
  [] -> ""
  ps -> ": " ++ intercalate ", " (map show ps)

prettyTags :: String -> [MoatType] -> String
prettyTags indents = go where
  go [] = ""
  go (Tag{..}:ts) = []
    ++ "\n"
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

prettyTagDisambiguator :: ()
  => Bool
     -- ^ disambiguate?
  -> String
     -- ^ indents
  -> String
     -- ^ parent type name
  -> String
prettyTagDisambiguator disambiguate indents parent
  = if disambiguate
      then []
        ++ indents
        ++ "enum "
        ++ parent
        ++ "Tag { }\n"
      else ""

labelCase :: Maybe String -> MoatType -> String
labelCase Nothing ty = prettyMoatType ty
labelCase (Just label) ty = "_ " ++ label ++ ": " ++ prettyMoatType ty

-- | Pretty-print a 'Ty'.
prettyMoatType :: MoatType -> String
prettyMoatType = \case
  Str -> "String"
  Unit -> "()"
  Bool -> "Bool"
  Character -> "Character"
  Tuple2 e1 e2 -> "(" ++ prettyMoatType e1 ++ ", " ++ prettyMoatType e2 ++ ")"
  Tuple3 e1 e2 e3 -> "(" ++ prettyMoatType e1 ++ ", " ++ prettyMoatType e2 ++ ", " ++ prettyMoatType e3 ++ ")"
  Optional e -> prettyMoatType e ++ "?"
  Result e1 e2 -> "Result<" ++ prettyMoatType e1 ++ ", " ++ prettyMoatType e2 ++ ">"
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
  Concrete ty tys -> ty
    ++ "<"
    ++ intercalate ", " (map prettyMoatType tys)
    ++ ">"
  Tag {..} -> tagParent ++ "." ++ tagName

prettyApp :: MoatType -> MoatType -> String
prettyApp t1 t2 = "(("
  ++ intercalate ", " (map prettyMoatType as)
  ++ ") -> "
  ++ prettyMoatType r
  ++ ")"
  where
    (as, r) = go t1 t2
    go e1 (App e2 e3) = case go e2 e3 of
      (args, ret) -> (e1 : args, ret)
    go e1 e2 = ([e1], e2)

prettyEnumCases :: String -> [(String, [(Maybe String, MoatType)])] -> String
prettyEnumCases indents = go
  where
    go = \case
      [] -> ""
      ((caseNm, []):xs) -> []
        ++ indents
        ++ "case "
        ++ caseNm
        ++ "\n"
        ++ go xs
      ((caseNm, cs):xs) -> []
        ++ indents
        ++ "case "
        ++ caseNm
        ++ "("
        ++ (intercalate ", " (map (uncurry labelCase) cs))
        ++ ")\n"
        ++ go xs

prettyStructFields :: String -> [(String, MoatType)] -> String
prettyStructFields indents = go
  where
    go [] = ""
    go ((fieldName,ty):fs) = indents ++ "let " ++ fieldName ++ ": " ++ prettyMoatType ty ++ "\n" ++ go fs

prettyPrivateTypes :: String -> [SwiftData] -> String
prettyPrivateTypes indents = go
  where
    go [] = ""
    go (s:ss) = indents ++ "private " ++ unlines (onLast (indents ++) (lines (prettySwiftData s))) ++ go ss

-- map a function over everything but the
-- first element.
onLast :: (a -> a) -> [a] -> [a]
onLast f [] = []
onLast f (x:xs) = x : map f xs
