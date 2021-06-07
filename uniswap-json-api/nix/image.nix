{ nixpkgs ? import ./nixpkgs.nix, ... }:
let
  pkgs = import nixpkgs { };
  drv = pkgs.callPackage ../. { };
in
pkgs.dockerTools.buildImage {
  name = "${drv.pname}-image";
  tag = "latest";
  contents = [ drv ];
  config.Cmd = [ "/bin/${drv.pname}" ];
}
