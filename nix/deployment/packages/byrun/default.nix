{ pkgs, ... }:

pkgs.stdenv.mkDerivation rec {
  pname = "byrun";
  version = "1.0";

  src = ./source;

  buildPhase = "";
  installPhase = ''
    mkdir -p $out/bin
    cp $src/* $out/bin
    chmod +x $out/bin/*
  '';
}
