{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted function" #-}
-- nub, error
module Moat.Pretty.Swift
  ( prettySwiftData,
    prettyMoatType,
  )
where

import Data.Char (toLower)
import Data.Functor ((<&>))
import Data.List (intercalate, nub)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
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
    let (rawValue, protocols) =
          enforcedEnumRawValueAndProtocols enumCases enumRawValue enumProtocols
     in prettyTypeDoc "" enumDoc []
          ++ "public enum "
          ++ prettyMoatTypeHeader enumName (addTyVarBounds enumTyVars protocols)
          ++ prettyRawValueAndProtocols rawValue protocols
          ++ " {"
          ++ newlineNonEmpty enumCases
          ++ prettyEnumCases indents rawValue enumEnumUnknownCase enumCases
          ++ newlineNonEmpty enumPrivateTypes
          ++ prettyPrivateTypes indents enumPrivateTypes
          ++ prettyTags indents enumTags
          ++ newlineNonEmpty enumTags
          ++ prettyEnumCoding indents enumName enumCases enumEnumUnknownCase enumSumOfProductEncodingOption
          ++ "}"
  MoatStruct {..} ->
    prettyTypeDoc "" structDoc []
      ++ "public struct "
      ++ prettyMoatTypeHeader structName (addTyVarBounds structTyVars structProtocols)
      ++ prettyRawValueAndProtocols Nothing structProtocols
      ++ " {"
      ++ newlineNonEmpty structFields
      ++ prettyStructFields indents structFields structDeprecatedFields
      ++ prettyStructInitializer indents structFields structDeprecatedFields
      ++ newlineNonEmpty structPrivateTypes
      ++ prettyPrivateTypes indents structPrivateTypes
      ++ prettyTags indents structTags
      ++ newlineNonEmpty structTags
      ++ "}"
  MoatAlias {..} ->
    prettyTypeDoc "" aliasDoc []
      ++ "public typealias "
      -- Swift aliases should not declare type parameters
      ++ prettyMoatTypeHeader aliasName []
      ++ " = "
      ++ prettyMoatTypeBase aliasTyp
  MoatNewtype {..} ->
    prettyTypeDoc "" newtypeDoc []
      ++ "public struct "
      ++ prettyMoatTypeHeader newtypeName (addTyVarBounds newtypeTyVars newtypeProtocols)
      ++ prettyRawValueAndProtocols Nothing newtypeProtocols
      ++ " {\n"
      ++ indents
      ++ if isConcrete newtypeField
        then
          "public let "
            ++ fieldName newtypeField
            ++ ": "
            ++ prettyMoatType (fieldType newtypeField)
            ++ "\n}"
        else
          "public typealias "
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

-- | For a plain (C-style) enum — one where every case is fieldless — enforce a
--   raw value and 'Codable' conformance. Swift synthesizes a verbose
--   keyed-object @Codable@ representation for raw-value-less enums (e.g.
--   @{"north": {}}@), so we pin a scalar raw value instead, which round-trips
--   as the bare tag (e.g. @"north"@).
--
--   An explicitly-supplied integer raw value is respected as-is; any other (or
--   absent) raw value defaults to 'Str'. 'Codable' is appended unless the user
--   already requested it.
--
--   Enums with associated values are returned unchanged: they cannot carry a
--   raw value and rely on the custom coding emitted by 'prettyEnumCoding'.
enforcedEnumRawValueAndProtocols ::
  [EnumCase] -> Maybe MoatType -> [Protocol] -> (Maybe MoatType, [Protocol])
enforcedEnumRawValueAndProtocols cases rawValue protocols
  | isCEnum cases =
      ( Just (fromMaybe Str rawValue)
      , protocols ++ [Codable | Codable `notElem` protocols]
      )
  | otherwise = (rawValue, protocols)

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
  Sendable -> "Sendable"
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
        ++ "public typealias "
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
        ++ "public enum "
        ++ parent
        ++ "Tag { }\n"
    else ""

-- | Lowercase the first character of an enum case name to produce an
--   idiomatic Swift case label. This only affects the Swift identifier; the
--   tag encoded to and decoded from the wire is left untouched.
swiftCaseLabel :: String -> String
swiftCaseLabel "" = ""
swiftCaseLabel (c : cs) = toLower c : cs

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

prettyEnumCases :: String -> Maybe MoatType -> Maybe String -> [EnumCase] -> String
prettyEnumCases indents rawValue unknown cases = go cases ++ unknownCase
  where
    -- For a 'Str' raw value, pin the raw value to the original case name
    -- whenever it differs from the lowercased Swift label, so the wire tag is
    -- preserved (e.g. @case aliceInChains = "AliceInChains"@). Integer raw
    -- values are left implicit for now.
    rawValueAssignment :: String -> String
    rawValueAssignment caseNm = case rawValue of
      Just Str | swiftCaseLabel caseNm /= caseNm -> " = \"" ++ caseNm ++ "\""
      _ -> ""

    go = \case
      [] -> ""
      (EnumCase caseNm caseDoc [] : xs) ->
        prettyTypeDoc indents caseDoc []
          ++ indents
          ++ "case "
          ++ swiftCaseLabel caseNm
          ++ rawValueAssignment caseNm
          ++ "\n"
          ++ go xs
      (EnumCase caseNm caseDoc cs : xs) ->
        prettyTypeDoc indents caseDoc cs
          ++ indents
          ++ "case "
          ++ swiftCaseLabel caseNm
          ++ "("
          ++ intercalate ", " (map labelCase cs)
          ++ ")\n"
          ++ go xs

    unknownCase = case unknown of
      Just caseNm -> indents ++ "case " ++ swiftCaseLabel caseNm ++ "\n"
      Nothing -> ""

prettyStructFields :: String -> [Field] -> [(String, Maybe String)] -> String
prettyStructFields indents fields deprecatedFields = go fields
  where
    deprecatedFieldsMap = Map.fromList deprecatedFields
    prettyField (Field fieldName fieldType _fieldDoc) =
      "public var "
        ++ fieldName
        ++ ": "
        ++ prettyMoatType fieldType
        ++ "\n"
    go [] = ""
    go (field@(Field fieldName _ fieldDoc) : fs) =
      case Map.lookup fieldName deprecatedFieldsMap of
        Just mComment ->
          indents
            ++ maybe "" (\comment -> "// " ++ comment ++ "\n") mComment
            ++ indents
            ++ "// "
            ++ prettyField field
            ++ go fs
        Nothing ->
          prettyTypeDoc indents fieldDoc []
            ++ indents
            ++ prettyField field
            ++ go fs

prettyStructInitializer :: String -> [Field] -> [(String, Maybe String)] -> String
prettyStructInitializer indents fields deprecatedFields =
  case activeFields of
    [] -> "" -- No initializer needed if there are no active fields
    _ ->
      "\n"
        ++ indents
        ++ "public init("
        ++ intercalate ", " (map prettyParam activeFields)
        ++ ") {\n"
        ++ concatMap (prettyAssignment indents) activeFields
        ++ indents
        ++ "}\n"
  where
    deprecatedFieldNames = map fst deprecatedFields
    activeFields = filter (\(Field name _ _) -> name `notElem` deprecatedFieldNames) fields

    isOptional :: MoatType -> Bool
    isOptional (Optional _) = True
    isOptional _ = False

    prettyParam :: Field -> String
    prettyParam (Field fieldName fieldType _) =
      fieldName ++ ": " ++ prettyMoatType fieldType ++ (if isOptional fieldType then " = nil" else "")

    prettyAssignment :: String -> Field -> String
    prettyAssignment indentStr (Field fieldName _ _) =
      indentStr ++ "    self." ++ fieldName ++ " = " ++ fieldName ++ "\n"

prettyNewtypeField :: String -> Field -> String -> String
prettyNewtypeField indents (Field alias fieldType _) fieldName =
  indents
    ++ "public let "
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
      "public enum CodingKeys: String, CodingKey {"
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
      "public init(from decoder: any Decoder) throws {"
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
                      ++ swiftCaseLabel caseNm
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
                      ++ swiftCaseLabel caseNm
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
                      ++ swiftCaseLabel caseNm
                  )
            EnumCase caseNm _ [Field "" caseTy _] ->
              "case \""
                ++ caseNm
                ++ "\":"
                ++ indent
                  ( "self = ."
                      ++ swiftCaseLabel caseNm
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
                      ++ swiftCaseLabel caseNm
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
          ++ indent ("self = ." ++ swiftCaseLabel caseNm)
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
      "public func encode(to encoder: any Encoder) throws {"
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
                  ++ swiftCaseLabel enumCaseName
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
                  ++ swiftCaseLabel enumCaseName
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
                  ++ swiftCaseLabel enumCaseName
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
                  ++ swiftCaseLabel enumCaseName
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
                  ++ swiftCaseLabel enumCaseName
                  ++ "("
                  ++ (intercalate ", " (enumCaseFields <&> \(Field {..}) -> fieldName))
                  ++ "):"
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
          ++ swiftCaseLabel caseNm
          ++ ":"
          ++ indent
            ( "throw EncodingError.invalidValue("
                ++ indent
                  ( "self,\n.init(codingPath: encoder.codingPath, debugDescription: \"Can't encode value: "
                      ++ parentName
                      ++ "."
                      ++ swiftCaseLabel caseNm
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
--   (similar to 'deriving stock'), of which there are currently four:
--
--   - 'Equatable'
--   - 'Hashable'
--   - 'Codable'
--   - 'Sendable'
--
--   See the [Swift documentation](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/protocols#Adopting-a-Protocol-Using-a-Synthesized-Implementation).
addTyVarBounds :: [String] -> [Protocol] -> [String]
addTyVarBounds tyVars protos =
  let isSynthesized :: Protocol -> Bool
      isSynthesized = \case
        Hashable -> True
        Codable -> True
        Equatable -> True
        Sendable -> True
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
