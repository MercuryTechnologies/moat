let
  flake-compat = import ./.nix/flake-compat.nix {
    src = ./.;
  };
in
flake-compat.defaultNix
