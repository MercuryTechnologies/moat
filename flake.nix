{
  description = "Generate swift and kotlin types from haskell types";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
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
          doJailbreak
          overrideCabal
          ;
        inherit
          (pkgs.haskell.lib.compose)
          appendPatches
          ;

        enableSeparateBinOutput = drv:
          if (pkgs.stdenv.hostPlatform.isDarwin && pkgs.stdenv.hostPlatform.isAarch64)
          then overrideCabal drv (_: {enableSeparateBinOutput = false;})
          else drv;

        haskellOverlays = {
          ghc902 = hself: hsuper: {
          };
          ghc924 = hself: hsuper: {
            # https://github.com/NixOS/nixpkgs/issues/140774
            ghcid = enableSeparateBinOutput hsuper.ghcid;
            hls-fourmolu-plugin = dontCheck hsuper.hls-fourmolu-plugin;
          };
          ghc942 = hself: hsuper: {
            # https://github.com/NixOS/nixpkgs/issues/140774
            ghcid = enableSeparateBinOutput hsuper.ghcid;
            hspec-contrib = dontCheck hsuper.hspec-contrib;
            hlint = hself.callHackage "hlint" "3.5" {};
            # Get rid of this once fourmolu supports GHC 9.4.
            fourmolu =
              appendPatches
              [
                # The GHC 9.4 pull request builds upon these unpublished changes,
                # which is why we include them.
                #
                # Also, there is no PR for these changes.  The fourmolu maintainers
                # apparently commit straight to master...
                (pkgs.fetchpatch {
                  url = "https://github.com/fourmolu/fourmolu/commit/2c11f555945e755cc6e805de482404d5484d53b6.patch";
                  sha256 = "sha256-bWJxOYQoLtsW7KaVbTxIxX+cFx9x3ekEmbA9NuJ3qtk=";
                })
                (pkgs.fetchpatch {
                  url = "https://github.com/fourmolu/fourmolu/commit/8eb7ef82676ab3bff9b249c74fc5d81dc23a2657.patch";
                  sha256 = "sha256-rlKDRv27EGnlStNiKEniZiKSib1Zr9E3hGT3QuU5xLI=";
                })
                (pkgs.fetchpatch {
                  url = "https://github.com/fourmolu/fourmolu/commit/14b1bc832716edaf4239c4e364836185f0aa816b.patch";
                  sha256 = "sha256-cfXh6ufxASbg2wXp/5ixfUNSC54O/eq8hx88lZKAJ18=";
                })
                (pkgs.fetchpatch {
                  url = "https://github.com/fourmolu/fourmolu/commit/e7fc4ec52aeeb50b3a886d1f55f4468ae6d46a46.patch";
                  sha256 = "sha256-d+hg/mOEAybq461VLkmCrwtFyHgX8Q+715yaJXHn7/g=";
                })
                (pkgs.fetchpatch {
                  url = "https://github.com/fourmolu/fourmolu/commit/809256014cb6f65d8cd3447c0502493f57d3e307.patch";
                  sha256 = "sha256-FdVj9MidRCfMUERVWvFV9k2rnvnorxJE7NpehNBQ8OQ=";
                })

                # The actual change to support GHC 9.4
                (pkgs.fetchpatch {
                  url = "https://github.com/fourmolu/fourmolu/pull/242.patch";
                  sha256 = "sha256-mWMtG+pkiNzmkXWVZhIILhhnPzQM070o6+hwyNZkY+0=";
                  includes = ["data/*" "src/*" "tests/*" "fourmolu.cabal"];
                })
              ]
              (doJailbreak hsuper.fourmolu_0_8_2_0);
            newtype-generics = doJailbreak hsuper.newtype-generics;
            conduit-extra = dontCheck hsuper.conduit-extra;
            hls-code-range-plugin = dontCheck hsuper.hls-code-range-plugin;
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
              ];
            }
        );
      in
        shells // {default = shells.${latestHaskell};}
    );
  };
}
