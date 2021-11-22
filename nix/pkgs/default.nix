{ pkgs, sources, plutus, haskell-nix, pre-commit-hooks-nix }:
let
  gitignore-nix = pkgs.callPackage plutus."gitignore.nix" { };

  compiler-nix-name = plutus.plutus-apps.haskell.compiler-nix-name;

  haskell = pkgs.callPackage ./haskell {
    inherit gitignore-nix sources haskell-nix;
    inherit compiler-nix-name;
  };

  hlint = plutus.plutus-apps.hlint;

  cabal-install = plutus.plutus-apps.cabal-install;

  stylish-haskell = plutus.plutus-apps.stylish-haskell;

  haskell-language-server = plutus.plutus-apps.haskell-language-server;

  purty-pre-commit = plutus.plutus-apps.purty-pre-commit;

in
{
  inherit haskell hlint cabal-install stylish-haskell;
  inherit haskell-language-server pre-commit-hooks-nix purty-pre-commit;
}
