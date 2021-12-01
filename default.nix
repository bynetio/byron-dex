let
  packages = import ./nix;

  inherit (packages) pkgs plutus dex easy-purescript-nix;
  project = dex.haskell.project;

  inherit (dex) haskell stylish-haskell devcontainer;

in
{
  inherit pkgs plutus dex easy-purescript-nix;

  inherit project;

  devcontainer =
    import ./nix/devcontainer/dex-devcontainer.nix { inherit pkgs dex; };
}
