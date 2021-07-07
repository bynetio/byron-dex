{ ... }:

let
  sources = import ../../vendor/plutus/nix/sources.nix { inherit pkgs; };
  pkgs = import sources.nixpkgs { };
  drv = pkgs.callPackage ../. { };
in
  pkgs.dockerTools.buildLayeredImage {
    name = "${drv.pname}-image";
    tag = "latest";
    contents = [ drv ];
    config.Cmd = [ "/bin/${drv.pname}" ];
  }
