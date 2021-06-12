{ nixpkgs ? import ./nixpkgs.nix, ... }:
let
  pkgs = import nixpkgs { };
  drv = pkgs.callPackage ../. { };
in
pkgs.mkShell {
  name = "${drv.pname}-shell";
  buildInputs = with pkgs; [ ghc cabal-install zlib drv ];
  shellHook = "echo '>> ${drv.pname}-shell <<'";
}
