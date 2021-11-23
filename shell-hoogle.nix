let
  packages = import ./.;
  inherit (packages) dex;
  inherit (dex) haskell;

in
haskell.project.shellFor {
  packages = ps: [ ps.dex ];
  withHoogle = true;
}
