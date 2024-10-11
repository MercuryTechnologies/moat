{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted function" #-}
-- nub, error
module Moat.Pretty.Swift
  ( prettySwiftData,
    prettyMoatType,
  )
where

import Data.Functor ((<&>))
import Data.List (intercalate, nub)
import qualified Data.Map as Map
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
      ++ prettyMoatTypeHeader enumName (addTyVarBounds enumTyVars enumProtocols)
      ++ prettyRawValueAndProtocols enumRawValue enumProtocols
      ++ " {"
      ++ newlineNonEmpty enumCases
      ++ prettyEnumCases indents enumEnumUnknownCase enumCases
      ++ newlineNonEmpty enumPrivateTypes
      ++ prettyPrivateTypes indents enumPrivateTypes
      ++ prettyTags indents enumTags
      ++ newlineNonEmpty enumTags
      ++ prettyEnumCoding indents enumName enumCases enumEnumUnknownCase enumSumOfProductEncodingOption
      ++ "}"
  MoatStruct {..} ->
    prettyTypeDoc "" structDoc []
      ++ "struct "
      ++ prettyMoatTypeHeader structName (addTyVarBounds structTyVars structProtocols)
      ++ prettyRawValueAndProtocols Nothing structProtocols
      ++ " {"
      ++ newlineNonEmpty structFields
      ++ prettyStructFields indents structFields structDeprecatedFields
      ++ newlineNonEmpty structPrivateTypes
      ++ prettyPrivateTypes indents structPrivateTypes
      ++ prettyTags indents structTags
      ++ newlineNonEmpty structTags
      ++ "}"
  MoatAlias {..} ->
    prettyTypeDoc "" aliasDoc []
      ++ "typealias "
      -- Swift aliases should not declare type parameters
      ++ prettyMoatTypeHeader aliasName []
      ++ " = "
      ++ prettyMoatTypeBase aliasTyp
  MoatNewtype {..} ->
    prettyTypeDoc "" newtypeDoc []
      ++ "struct "
      ++ prettyMoatTypeHeader newtypeName (addTyVarBounds newtypeTyVars newtypeProtocols)
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
            ++ case fieldType newtypeField of
              Optional t -> prettyMoatType t
              t -> prettyMoatType t
            ++ ">\n"
            ++ prettyNewtypeField indents newtypeField newtypeName
            ++ "}"
  where
    indents = replicate indent ' '

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

prettyProtocol :: Protocol -> String
prettyProtocol = \case
  Hashable -> "Hashable"
  Codable -> "Codable"
  Equatable -> "Equatable"
  OtherProtocol s -> s

prettyProtocols :: [Protocol] -> String
prettyProtocols = \case
  [] -> ""
  ps -> intercalate ", " (prettyProtocol <$> ps)

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

-- | Pretty-print a 'MoatType', omitting type parameters.
prettyMoatTypeBase :: MoatType -> String
prettyMoatTypeBase = \case
  Result _ _ -> "Result"
  Set _ -> "Set"
  Dictionary _ _ -> "Dictionary"
  Concrete ty _ -> ty
  ty -> prettyMoatType ty

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

prettyEnumCases :: String -> Maybe String -> [EnumCase] -> String
prettyEnumCases indents unknown cases = go cases ++ unknownCase
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

    unknownCase = case unknown of
      Just caseNm -> indents ++ "case " ++ caseNm ++ "\n"
      Nothing -> ""

prettyStructFields :: String -> [Field] -> [(String, Maybe String)] -> String
prettyStructFields indents fields deprecatedFields = go fields
  where
    deprecatedFieldsMap = Map.fromList deprecatedFields
    prettyField (Field fieldName fieldType _fieldDoc) =
      indents
        ++ "var "
        ++ fieldName
        ++ ": "
        ++ prettyMoatType fieldType
        ++ "\n"
    go [] = ""
    go (field@(Field fieldName _ fieldDoc) : fs) =
      case Map.lookup fieldName deprecatedFieldsMap of
        Just mComment ->
          maybe "" (\comment -> "// " ++ comment ++ "\n") mComment
            ++ "//"
            ++ prettyField field
            ++ go fs
        Nothing ->
          prettyTypeDoc indents fieldDoc []
            ++ prettyField field
            ++ go fs

prettyNewtypeField :: String -> Field -> String -> String
prettyNewtypeField indents (Field alias fieldType _) fieldName =
  indents
    ++ "let "
    ++ alias
    ++ ": "
    ++ fieldName
    ++ "Tag"
    ++ case fieldType of
      Optional _ -> "?"
      _ -> ""
    ++ "\n"

prettyPrivateTypes :: String -> [MoatData] -> String
prettyPrivateTypes indents = go
  where
    go [] = ""
    go (s : ss) = indents ++ "private " ++ unlines (onLast (indents ++) (lines (prettySwiftData s))) ++ go ss

prettyEnumCoding ::
  String ->
  String ->
  [EnumCase] ->
  Maybe String ->
  SumOfProductEncodingOptions ->
  String
prettyEnumCoding indents parentName cases unknownCase SumOfProductEncodingOptions {..}
  | isCEnum cases = "" -- TODO Perhaps add Codable implementation for these
  | otherwise =
      indent $
        prettyCodingKeys
          ++ "\n\n"
          ++ prettyInit
          ++ "\n\n"
          ++ prettyEncode
  where
    indent :: String -> String
    indent = indentBy indents

    prettyCodingKeys :: String
    prettyCodingKeys =
      "enum CodingKeys: String, CodingKey {"
        ++ indent
          ( case encodingStyle of
              TaggedObjectStyle -> prettyTaggedCodingKeys
              TaggedFlatObjectStyle -> prettyFlatCodingKeys
          )
        ++ "}"

    prettyTaggedCodingKeys :: String
    prettyTaggedCodingKeys =
      "case "
        ++ tagFieldName
        ++ "\n"
        ++ "case "
        ++ contentsFieldName

    -- We need all possible keys in the payload
    prettyFlatCodingKeys =
      let names = nub $ filter (not . null) (cases >>= enumCaseFields <&> fieldName)
       in "case "
            ++ tagFieldName
            ++ "\n"
            ++ intercalate "\n" (map ("case " ++) names)

    prettyInit :: String
    prettyInit =
      "init(from decoder: any Decoder) throws {"
        ++ indent
          ( "let container = try decoder.container(keyedBy: CodingKeys.self)\n"
              ++ "let discriminator = try container.decode(String.self, forKey: ."
              ++ tagFieldName
              ++ ")\n"
              ++ "switch discriminator {"
              ++ indent
                ( case encodingStyle of
                    TaggedObjectStyle -> prettyTaggedInitCases
                    TaggedFlatObjectStyle -> prettyFlatInitCases
                    ++ prettyInitUnknownCase
                )
              ++ "}"
          )
        ++ "}"

    -- TaggedObjectStyle payloads have a single tag and contents field.
    prettyTaggedInitCases :: String
    prettyTaggedInitCases =
      concatMap
        ( \case
            EnumCase caseNm _ [Field _ caseTy _] ->
              "case \""
                ++ caseNm
                ++ "\":"
                ++ indent
                  ( "self = ."
                      ++ caseNm
                      ++ "(try container.decode("
                      ++ prettyMoatType caseTy
                      ++ ".self, forKey: ."
                      ++ contentsFieldName
                      ++ "))"
                  )
            EnumCase caseNm _ [] ->
              "case \""
                ++ caseNm
                ++ "\":"
                ++ indent
                  ( "self = ."
                      ++ caseNm
                  )
            EnumCase caseNm _ _ ->
              error $
                "prettyTaggedEnumCoding: The data constructor "
                  <> caseNm
                  <> " can have zero or one concrete type constructor when using TaggedObjectStyle!"
        )
        cases

    -- TaggedFlatObjectStyle payloads have a tag field and 0 or more additional fields
    -- that are decoded directly into the case type.
    prettyFlatInitCases :: String
    prettyFlatInitCases =
      concatMap
        ( \case
            EnumCase caseNm _ [] ->
              "case \""
                ++ caseNm
                ++ "\":"
                ++ indent
                  ( "self = ."
                      ++ caseNm
                  )
            EnumCase caseNm _ [Field "" caseTy _] ->
              "case \""
                ++ caseNm
                ++ "\":"
                ++ indent
                  ( "self = ."
                      ++ caseNm
                      ++ "(try "
                      ++ prettyMoatType caseTy
                      ++ ".init(from: decoder))"
                  )
            EnumCase caseNm _ fields ->
              "case \""
                ++ caseNm
                ++ "\":"
                ++ indent
                  ( "self = ."
                      ++ caseNm
                      ++ "("
                      ++ indent
                        ( intercalate
                            ",\n"
                            ( fields <&> \(Field {..}) ->
                                "try container.decode("
                                  ++ prettyMoatType fieldType
                                  ++ ".self, forKey: ."
                                  ++ fieldName
                                  ++ ")"
                            )
                        )
                      ++ ")"
                  )
        )
        cases

    prettyInitUnknownCase :: String
    prettyInitUnknownCase = case unknownCase of
      Just caseNm ->
        "default:"
          ++ indent ("self = ." ++ caseNm)
      Nothing ->
        "default:"
          ++ indent
            ( "throw DecodingError.typeMismatch("
                ++ indent
                  ( "CodingKeys.self,\n"
                      ++ ".init(codingPath: decoder.codingPath, debugDescription: \"Can't decode unknown "
                      ++ tagFieldName
                      ++ ": "
                      ++ parentName
                      ++ ".\\(discriminator)\")"
                  )
                ++ ")"
            )

    prettyEncode :: String
    prettyEncode =
      "func encode(to encoder: any Encoder) throws {"
        ++ indent
          ( "var container = encoder.container(keyedBy: CodingKeys.self)\n"
              ++ "switch (self) {"
              ++ indent
                ( case encodingStyle of
                    TaggedObjectStyle -> prettyEncodeTaggedCases
                    TaggedFlatObjectStyle -> prettyEncodeFlatCases
                    ++ prettyEncodeUnknownCase
                )
              ++ "}\n"
          )
        ++ "}"

    prettyEncodeTaggedCases :: String
    prettyEncodeTaggedCases =
      concatMap
        ( \(EnumCase {..}) ->
            case enumCaseFields of
              [] ->
                "case ."
                  ++ enumCaseName
                  ++ ":"
                  ++ indent
                    ( "try container.encode(\""
                        ++ enumCaseName
                        ++ "\", forKey: ."
                        ++ tagFieldName
                        ++ ")"
                    )
              [Field "" _ _] ->
                "case let ."
                  ++ enumCaseName
                  ++ "("
                  ++ contentsFieldName
                  ++ "):"
                  ++ indent
                    ( "try container.encode(\""
                        ++ enumCaseName
                        ++ "\", forKey: ."
                        ++ tagFieldName
                        ++ ")\ntry container.encode("
                        ++ contentsFieldName
                        ++ ", forKey: ."
                        ++ contentsFieldName
                        ++ ")"
                    )
              _ ->
                error $
                  "prettyTaggedEnumCoding: The data constructor "
                    <> enumCaseName
                    <> " can have zero or one concrete type constructor when using TaggedObjectStyle!"
        )
        cases

    prettyEncodeFlatCases :: String
    prettyEncodeFlatCases =
      concatMap
        ( \(EnumCase {..}) ->
            case enumCaseFields of
              [] ->
                "case ."
                  ++ enumCaseName
                  ++ ":"
                  ++ indent
                    ( "try container.encode(\""
                        ++ enumCaseName
                        ++ "\", forKey: ."
                        ++ tagFieldName
                        ++ ")"
                    )
              [Field "" _ _] ->
                "case let ."
                  ++ enumCaseName
                  ++ "(value):"
                  ++ indent
                    ( "try container.encode(\""
                        ++ enumCaseName
                        ++ "\", forKey: ."
                        ++ tagFieldName
                        ++ ")\n"
                        ++ "try value.encode(to: encoder)"
                    )
              _ ->
                "case let ."
                  ++ enumCaseName
                  ++ ":"
                  ++ indent
                    ( "try container.encode(\""
                        ++ enumCaseName
                        ++ "\", forKey: ."
                        ++ tagFieldName
                        ++ ")\n"
                        ++ intercalate
                          "\n"
                          ( enumCaseFields <&> \(Field {..}) ->
                              "try container.encode("
                                ++ fieldName
                                ++ ", forKey: ."
                                ++ fieldName
                                ++ ")"
                          )
                    )
        )
        cases

    prettyEncodeUnknownCase :: String
    prettyEncodeUnknownCase = case unknownCase of
      Just caseNm ->
        "case ."
          ++ caseNm
          ++ ":"
          ++ indent
            ( "throw EncodingError.invalidValue("
                ++ indent
                  ( "self,\n.init(codingPath: encoder.codingPath, debugDescription: \"Can't encode value: "
                      ++ parentName
                      ++ "."
                      ++ caseNm
                      ++ "\")"
                  )
                ++ ")"
            )
      Nothing -> ""

-- map a function over everything but the
-- first element.
onLast :: (a -> a) -> [a] -> [a]
onLast _ [] = []
onLast f (x : xs) = x : map f xs

-- | Copy protocols from the parent type to upper bounds of generic type
--   parameters.
--
--   This is needed for protocols with compiler-synthesized implementations
--   (similar to 'deriving stock'), of which there are currently three:
--
--   - 'Equatable'
--   - 'Hashable'
--   - 'Codable'
--
--   See the [Swift documentation](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/protocols#Adopting-a-Protocol-Using-a-Synthesized-Implementation).
addTyVarBounds :: [String] -> [Protocol] -> [String]
addTyVarBounds tyVars protos =
  let isSynthesized :: Protocol -> Bool
      isSynthesized = \case
        Hashable -> True
        Codable -> True
        Equatable -> True
        OtherProtocol _ -> False
      synthesizedProtos = filter isSynthesized protos
      bounds = ": " ++ intercalate " & " (map prettyProtocol synthesizedProtos)
   in case synthesizedProtos of
        [] -> tyVars
        _ -> map (++ bounds) tyVars

newlineNonEmpty :: [a] -> String
newlineNonEmpty [] = ""
newlineNonEmpty _ = "\n"

indentBy :: String -> String -> String
indentBy indents str = "\n" ++ unlines (map indentLine $ lines str)
  where
    indentLine :: String -> String
    indentLine "" = ""
    indentLine ln = indents ++ ln

isCEnum :: [EnumCase] -> Bool
isCEnum = all ((== []) . enumCaseFields)
