let
  packages = import ./.;
  inherit (packages) uniswap;
  inherit (uniswap) haskell;

in
haskell.project.shellFor {
  packages = ps: [ ps.uniswap ];
  withHoogle = true;
}
