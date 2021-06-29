# dex

Fetch plutus submodule:
```
git submodule update --init
```

Build plutus packages:
```
nix-shell
bin/build-plutus
```

Generate hoogle documentation database:
```
nix-shell
bin/build-hoogle
```

Start [plutus hoogle](http://localhost:8080/):
```
nix-shell
bin/start-hoogle
```

Generate ctags
```bash
find vendor/plutus/ -name "*.*hs" \
  |  grep -v 'fomega/app/Main.hs' \
  | grep -v 'nofib/src/Plutus/Benchmark' \
  | xargs  hs-tags --cabal uniswap/uniswap.cabal -c 
```

# vscode integration

Plugins
* [vscode-haskell](https://github.com/haskell/vscode-haskell)
* [nix-env-selector](https://github.com/arrterian/nix-env-selector)
* [ctagsx](https://github.com/jtanx/ctagsx)

