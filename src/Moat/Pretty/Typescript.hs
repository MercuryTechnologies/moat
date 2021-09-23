{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Moat.Pretty.Typescript
  ( prettyTypescriptData,
  )
where

import qualified Data.Char as Char
import Data.List (intercalate)
import Moat.Types

prettyTypescriptData :: MoatData -> String
prettyTypescriptData = \case
  MoatStruct {..} ->
    prettyStruct structName structFields indents
  MoatEnum {..} ->
    prettyEnum enumName enumCases indents
  MoatNewtype {..} ->
    prettyStruct newtypeName [newtypeField] indents
  MoatAlias {..} ->
    prettyAlias aliasName aliasTyVars aliasTyp
  where
    indent = 2
    indents = replicate indent ' '

prettyStruct ::
  -- | name
  String ->
  -- | cases
  [(String, MoatType)] ->
  -- | indents
  String ->
  String
prettyStruct name cases indents =
  "export type T"
    ++ upperFirst
    ++ " = I"
    ++ upperFirst
    ++ "\n\n"
    ++ "type I"
    ++ upperFirst
    ++ " = {\n"
    ++ prettyStructCases indents cases
    ++ "\n}"
  where
    upperFirst = toUpperFirst name

prettyStructCases ::
  String ->
  [(String, MoatType)] ->
  String
prettyStructCases indents = intercalate "\n" . foldMap prettyStructCase
  where
    prettyStructCase :: (String, MoatType) -> [String]
    prettyStructCase (caseNm, typ) =
      [ indents
          ++ caseNm
          ++ case typ of
            Optional _ -> "?: "
            _ -> ": "
          ++ prettyMoatType typ
      ]

prettyEnum ::
  -- | name
  String ->
  -- | cases
  [(String, [(Maybe String, MoatType)])] ->
  -- | indents
  String ->
  String
prettyEnum name cases indents =
  "export type T"
    ++ toUpperFirst name
    ++ " = "
    ++ if isSimpleEnum cases
      then prettySimpleEnumCases cases
      else prettyEnumCases indents cases
  where
    isSimpleEnum :: Eq b => [(a, [b])] -> Bool
    isSimpleEnum = all ((== []) . snd)

prettySimpleEnumCases ::
  [(String, [(Maybe String, MoatType)])] ->
  String
prettySimpleEnumCases = intercalate " | " . foldMap prettyEnumCase
  where
    prettyEnumCase :: (String, [(Maybe String, MoatType)]) -> [String]
    prettyEnumCase (caseNm, _) = ["'" <> caseNm <> "'"]

prettyEnumCases ::
  String ->
  [(String, [(Maybe String, MoatType)])] ->
  String
prettyEnumCases indents cases =
  exportLine
    ++ "\n\n"
    ++ taggedUnions
  where
    exportLine = intercalate " | " . foldMap prettyInterface $ fst <$> cases
    prettyInterface :: String -> [String]
    prettyInterface caseNm = ["I" ++ toUpperFirst caseNm]

    taggedUnions = intercalate "\n\n" . foldMap prettyUnion $ cases
    prettyUnion :: (String, [(Maybe String, MoatType)]) -> [String]
    prettyUnion (caseNm, xs) =
      [ "type I"
          ++ toUpperFirst caseNm
          ++ " = {\n"
          ++ indents
          ++ "tag: \""
          ++ caseNm
          ++ "\"\n"
          ++ indents
          ++ "contents: "
          ++ ( case xs of
                 [tup] ->
                   intercalate "" (prettyContents "" tup)
                 _ ->
                   "{\n"
                     ++ intercalate "\n" (foldMap (prettyContents indents) xs)
                     ++ "\n"
                     ++ indents
                     ++ "}"
             )
          ++ "\n}"
      ]
    prettyContents :: String -> (Maybe String, MoatType) -> [String]
    prettyContents inds =
      pure . ((inds <> inds) <>) . \case
        (Nothing, Concrete conc _) -> "T" <> toUpperFirst conc
        (Nothing, typ) -> prettyMoatType typ
        (Just name, Concrete conc _) -> name <> ": " <> "T" <> toUpperFirst conc
        (Just name, typ) -> name <> ": " <> prettyMoatType typ

prettyAlias ::
  String ->
  [String] ->
  MoatType ->
  String
prettyAlias name _ typ = "type " ++ name ++ " = " ++ prettyMoatType typ

prettyMoatType :: MoatType -> String
prettyMoatType = \case
  Str -> "string"
  Unit -> "null"
  Bool -> "boolean"
  Character -> "string"
  Tuple2 e1 e2 -> "[" ++ prettyMoatType e1 ++ ", " ++ prettyMoatType e2 ++ "]"
  Tuple3 e1 e2 e3 -> "[" ++ prettyMoatType e1 ++ ", " ++ prettyMoatType e2 ++ ", " ++ prettyMoatType e3 ++ "]"
  Optional e -> prettyMoatType e
  Result e1 e2 -> prettyMoatType e1 ++ " | " ++ prettyMoatType e2
  Set e -> "Set<" ++ prettyMoatType e ++ ">"
  Dictionary e1 e2 -> "Map<" ++ prettyMoatType e1 ++ ", " ++ prettyMoatType e2 ++ ">"
  Array e -> prettyMoatType e ++ "[]"
  -- App is special, we recurse until we no longer
  -- any applications.
  App e1 e2 -> prettyApp e1 e2
  I -> "number"
  I8 -> "number"
  I16 -> "number"
  I32 -> "number"
  I64 -> "number"
  U -> "number"
  U8 -> "number"
  U16 -> "number"
  U32 -> "number"
  U64 -> "number"
  F32 -> "number"
  F64 -> "number"
  Decimal -> "number"
  BigInt -> "number"
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

toUpperFirst :: String -> String
toUpperFirst = \case
  [] -> []
  (c : cs) -> Char.toUpper c : cs
