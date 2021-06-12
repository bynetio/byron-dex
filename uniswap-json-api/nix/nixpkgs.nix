let
  host = import <nixpkgs> { };
  pin = host.lib.importJSON ./nixpkgs.json;
  pkgs = host.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    inherit (pin) rev sha256;
  };
in
pkgs
