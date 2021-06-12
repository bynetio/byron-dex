{ nixpkgs ? import ./nix/nixpkgs.nix, ... }:
let
  pkgs = import nixpkgs { };
  drv = pkgs.haskellPackages.callCabal2nix "uniswap-json-api" ./. { };
in
drv
