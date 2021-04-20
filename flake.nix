{
  description = "Generate swift and kotlin types from haskell types";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-darwin" "x86_64-linux" ];

      eachSystem = nixpkgs.lib.genAttrs systems;

      pkgsBySystem = eachSystem (system: nixpkgs.legacyPackages.${system});

    in
    {
      defaultPackage = eachSystem (system:
        let
          pkgs = pkgsBySystem.${system};
          moat =
            { mkDerivation, base, bytestring, case-insensitive
            , containers, hpack, hspec, hspec-golden, lib, mtl, primitive
            , template-haskell, text, th-abstraction, time
            , unordered-containers, uuid-types, vector
            }:
            mkDerivation {
              pname = "moat";
              version = "0.1";
              src = ./.;
              libraryHaskellDepends = [
                base bytestring case-insensitive containers mtl primitive
                template-haskell text th-abstraction time unordered-containers
                uuid-types vector
              ];
              libraryToolDepends = [ hpack ];
              testHaskellDepends = [
                base bytestring case-insensitive containers hspec hspec-golden mtl
                primitive template-haskell text th-abstraction time
                unordered-containers uuid-types vector
              ];
              prePatch = "hpack";
              homepage = "https://github.com/chessai/moat#readme";
              description = "Generate swift and kotlin types from haskell types";
              license = lib.licenses.mit;
            };
        in
        pkgs.haskellPackages.callPackage moat { }
      );

      devShell = eachSystem (system:
        let
          pkgs = pkgsBySystem.${system};
        in
          pkgs.mkShell {
            name = "moat-shell";
            inputsFrom = [ self.defaultPackage.${system} ];
            buildInputs = with pkgs.haskellPackages; [
              cabal-install
              ghc
              ghcid
              hpack
            ];
          }
      );
    };
}
