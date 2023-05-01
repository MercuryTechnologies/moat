{
  description = "Generate swift and kotlin types from haskell types";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    systems = ["aarch64-darwin" "x86_64-darwin" "x86_64-linux"];

    haskells = ["ghc902" "ghc924" "ghc942"];

    eachSystem = nixpkgs.lib.genAttrs systems;

    eachHaskell = nixpkgs.lib.genAttrs haskells;

    latestHaskell = nixpkgs.lib.last haskells;

    pkgsBySystem = eachSystem (system: nixpkgs.legacyPackages.${system});
  in {
    packages = eachSystem (
      system: let
        pkgs = pkgsBySystem.${system};

        moats = builtins.listToAttrs (map (haskell: {
            name = "moat-${haskell}";
            value = pkgs.haskell.packages.${haskell}.callPackage ./moat.nix {};
          })
          haskells);
      in
        moats // {default = moats."moat-${latestHaskell}";}
    );

    devShells = eachSystem (
      system: let
        pkgs = pkgsBySystem.${system};
        inherit
          (pkgs.haskell.lib)
          dontCheck
          overrideCabal
          ;

        enableSeparateBinOutput = drv:
          if (pkgs.stdenv.hostPlatform.isDarwin && pkgs.stdenv.hostPlatform.isAarch64)
          then overrideCabal drv (_: {enableSeparateBinOutput = false;})
          else drv;

        haskellOverlays = {
          ghc902 = hself: hsuper: {
            # Wants cabal == 3.6
            fourmolu = hsuper.fourmolu.overrideScope (lself: lsuper: {
              Cabal = lself.Cabal_3_6_3_0;
            });
          };
          ghc924 = hself: hsuper: {
            # https://github.com/NixOS/nixpkgs/issues/140774
            ghcid = enableSeparateBinOutput hsuper.ghcid;
            hls-fourmolu-plugin = dontCheck hsuper.hls-fourmolu-plugin;
          };
          ghc942 = hself: hsuper: {
            # https://github.com/NixOS/nixpkgs/issues/140774
            ghcid = dontCheck (enableSeparateBinOutput hsuper.ghcid);
          };
        };

        shells = eachHaskell (
          haskell: let
            hsPkgs = pkgs.haskell.packages.${haskell}.override (prev: {
              overrides = haskellOverlays.${haskell};
            });
          in
            pkgs.mkShell {
              name = "moat-${haskell}-shell";
              inputsFrom = [self.packages.${system}."moat-${haskell}"];
              nativeBuildInputs = [
                hsPkgs.cabal2nix
                hsPkgs.cabal-install
                hsPkgs.ghc
                hsPkgs.ghcid
                hsPkgs.haskell-language-server
                hsPkgs.hlint
                hsPkgs.hpack
                hsPkgs.fourmolu
                pkgs.alejandra
              ];
            }
        );
      in
        shells // {default = shells.${latestHaskell};}
    );
  };
}
