{ mkDerivation, base, bytestring, case-insensitive, containers
, hpack, hspec, hspec-discover, hspec-golden, lib, mtl, primitive
, template-haskell, text, th-abstraction, th-compat, time
, unordered-containers, uuid-types, vector
}:
mkDerivation {
  pname = "moat";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers mtl primitive
    template-haskell text th-abstraction th-compat time
    unordered-containers uuid-types vector
  ];
  libraryToolDepends = [ hpack hspec-discover ];
  testHaskellDepends = [
    base bytestring case-insensitive containers hspec hspec-discover
    hspec-golden mtl primitive template-haskell text th-abstraction
    th-compat time unordered-containers uuid-types vector
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  homepage = "https://github.com/chessai/moat#readme";
  description = "Generate swift and kotlin types from haskell types";
  license = lib.licenses.mit;
}
