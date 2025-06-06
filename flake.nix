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

    haskells = ["ghc90" "ghc92" "ghc94" "ghc96"];

    eachSystem = nixpkgs.lib.genAttrs systems;

    eachHaskell = nixpkgs.lib.genAttrs haskells;

    latestHaskell = nixpkgs.lib.last haskells;

    pkgsBySystem = eachSystem (system: nixpkgs.legacyPackages.${system});

    haskellPackages = eachSystem (
      system:
        eachHaskell (
          haskell:
            pkgsBySystem.${system}.haskell.packages.${haskell}
        )
    );
  in {
    packages = eachSystem (
      system: let
        moats =
          nixpkgs.lib.mapAttrs'
          (n: v: {
            name = "moat-${n}";
            value = v;
          })
          (eachHaskell (
            haskell:
              haskellPackages.${system}.${haskell}.callPackage ./moat.nix {}
          ));
      in
        moats // {default = moats."moat-${latestHaskell}";}
    );

    devShells = eachSystem (
      system: let
        pkgs = pkgsBySystem.${system};

        shells = eachHaskell (
          haskell: let
            hsPkgs = haskellPackages.${system}.${haskell};
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
