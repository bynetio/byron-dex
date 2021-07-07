{ ... }:

let
  sources = import ../vendor/plutus/nix/sources.nix { inherit pkgs; };
  pkgs = import sources.nixpkgs { };
  drv = pkgs.haskellPackages.callCabal2nix "uniswap-json-api" ./. { };
in
  drv
