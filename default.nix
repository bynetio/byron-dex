let
  packages = import ./nix;

  inherit (packages) pkgs plutus uniswap easy-purescript-nix;
  project = uniswap.haskell.project;

  inherit (uniswap) haskell stylish-haskell devcontainer;

in
{
  inherit pkgs plutus uniswap easy-purescript-nix;

  inherit project;

  devcontainer = import ./nix/devcontainer/uniswap-devcontainer.nix {
    inherit pkgs uniswap;
  };
}
