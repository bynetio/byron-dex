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
