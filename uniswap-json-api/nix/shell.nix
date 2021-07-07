{ ... }:

let
  sources = import ../../vendor/plutus/nix/sources.nix { inherit pkgs; };
  pkgs = import sources.nixpkgs { };
  drv = pkgs.callPackage ../. { };
in
  pkgs.mkShell {
    name = "${drv.pname}-shell";
    buildInputs = with pkgs; [ ghc cabal-install zlib drv ];
    shellHook = "echo '>> ${drv.pname}-shell <<'";
  }
