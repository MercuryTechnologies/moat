{
  description = "Generate swift and kotlin types from haskell types";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=536fe36e23ab0fc8b7f35c24603422eee9fc17a2";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-darwin" "x86_64-linux" ];

      haskells = [ "ghc884" "ghc8103" ];

      eachSystem = nixpkgs.lib.genAttrs systems;

      pkgsBySystem = eachSystem (system: nixpkgs.legacyPackages.${system});

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

          homepage = "https://github.com/chessai/moat#readme";
          description = "Generate swift and kotlin types from haskell types";
          license = lib.licenses.mit;
        };

    in
    {
      defaultPackage = eachSystem (system:
        self.packages.${system}.moat-ghc8103
      );

      packages = eachSystem (system:
        let
          pkgs = pkgsBySystem.${system};

          moats = map (haskell: {
            name = "moat-${haskell}";
            value = pkgs.haskell.packages.${haskell}.callPackage moat { };
          }) haskells;
        in
        builtins.listToAttrs moats
      );

      devShell = eachSystem (system:
        let
          pkgs = pkgsBySystem.${system};
        in
          pkgs.mkShell {
            name = "moat-shell";
            inputsFrom = [ self.defaultPackage.${system} ];
            buildInputs = with pkgs.haskell.packages.ghc8103; [
              cabal-install
              ghc
              ghcid
              hpack
              ormolu
            ];
          }
      );
    };
}
