let
  packages = import ./nix;

  inherit (packages) pkgs dex easy-purescript-nix;
  project = dex.haskell.project;

  inherit (dex) haskell stylish-haskell devcontainer;

in
{
  inherit pkgs dex easy-purescript-nix;

  inherit project;

  devcontainer = import ./nix/devcontainer/dex-devcontainer.nix {
    inherit pkgs dex;
  };
}
