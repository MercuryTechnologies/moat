{
  description = "Generate swift and kotlin types from haskell types";

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" "x86_64-darwin" "x86_64-linux" ];

      haskells = [ "ghc8107" "ghc902" "ghc922" ];

      eachSystem = nixpkgs.lib.genAttrs systems;

      eachHaskell = nixpkgs.lib.genAttrs haskells;

      latestHaskell = nixpkgs.lib.last haskells;

      pkgsBySystem = eachSystem (system: nixpkgs.legacyPackages.${system});

    in
    {
      packages = eachSystem (system:
        let
          pkgs = pkgsBySystem.${system};

          moats = builtins.listToAttrs (map (haskell: {
            name = "moat-${haskell}";
            value = pkgs.haskell.packages.${haskell}.callPackage ./moat.nix { };
          }) haskells);
        in
        moats // { default = moats."moat-${latestHaskell}"; }
      );

      devShells = eachSystem (system:
        let
          pkgs = pkgsBySystem.${system};
          shells = eachHaskell (haskell:
            pkgs.mkShell {
              name = "moat-${haskell}-shell";
              inputsFrom = [ self.packages.${system}."moat-${haskell}" ];
              nativeBuildInputs = with pkgs.haskell.packages.${haskell}; [
                cabal2nix
                cabal-install
                ghc
                ghcid
                haskell-language-server
                hlint
                hpack
                ormolu
              ];
            }
          );
        in
        shells // { default = shells.${latestHaskell}; }
      );
    };
}
