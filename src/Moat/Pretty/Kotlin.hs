{-# language
    LambdaCase
  , RecordWildCards
  #-}

module Moat.Pretty.Kotlin
  ( prettyKotlinData
  ) where

import Data.List (intercalate)
import Moat.Types
import qualified Data.Char as Char

prettyKotlinData :: MoatData -> String
prettyKotlinData = \case

  MoatStruct{..} -> ""
    ++ prettyInterfaces structInterfaces
    ++ "data class "
    ++ prettyMoatTypeHeader structName structTyVars
    ++ "("
    ++ newlineNonEmpty structFields
    ++ prettyStructFields indents structFields
    ++ ")"

  MoatEnum{..} -> ""
    ++ prettyInterfaces enumInterfaces
    ++ "sealed class "
    ++ prettyMoatTypeHeader enumName enumTyVars
    ++ "("
    ++ newlineNonEmpty enumCases
    ++ prettyEnumCases enumName indents enumCases
    ++ ")"

  MoatNewtype{..} -> ""
    ++ prettyInterfaces newtypeInterfaces
    ++ "inline class "
    ++ prettyMoatTypeHeader newtypeName newtypeTyVars
    ++ "(val "
    ++ fst newtypeField
    ++ ": "
    ++ prettyMoatType (snd newtypeField)
    ++ ")"

  MoatAlias{..} -> ""
    ++ "typealias "
    ++ prettyMoatTypeHeader aliasName aliasTyVars
    ++ " = "
    ++ prettyMoatType aliasTyp

  where
    indent = 4
    indents = replicate indent ' '

    newlineNonEmpty [] = ""
    newlineNonEmpty _ = "\n"

prettyStructFields :: String -> [(String, MoatType)] -> String
prettyStructFields indents = go
  where
    go [] = ""
    go ((fieldName, ty):fs) = indents ++ "val " ++ fieldName ++ ": " ++ prettyMoatType ty ++ ",\n" ++ go fs

prettyEnumCases :: String -> String -> [(String, [(Maybe String, MoatType)])] -> String
prettyEnumCases typName indents = go
  where
    toUpperFirst = \case
      [] -> []
      (c : cs) -> Char.toUpper c : cs

    go = \case
      [] -> ""
      ((caseNm, []):xs) -> []
        ++ indents
        ++ "data class "
        ++ toUpperFirst caseNm
        ++ "() : "
        ++ typName
        ++ "\n"
        ++ go xs
      ((caseNm, cs):xs) -> []
        ++ indents
        ++ "data class "
        ++ toUpperFirst caseNm
        ++ "(\n"
        ++ intercalate ",\n"
             ( map ((++) indents)
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

prettyInterface :: Interface -> String
prettyInterface = \case
  Serializable -> "Serializable"
  _ -> error "TODO"

prettyInterfaces :: [Interface] -> String
prettyInterfaces = concatMap (\i -> "@" ++ prettyInterface i ++ "\n")

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
  Array e -> "Array<" ++ prettyMoatType e ++ ">"
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
  Concrete ty tys -> ty
    ++ "<"
    ++ intercalate ", " (map prettyMoatType tys)
    ++ ">"
  Tag {..} -> tagName

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

