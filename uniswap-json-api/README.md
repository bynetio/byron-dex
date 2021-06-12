# json-api-template

#### build docker image

```
docker load < $(nix-build --no-out-link nix/image.nix)
```

#### run nix shell

```
nix-shell nix/shell.nix
```

#### update cabal-based project configuration for nix environment

```
cabal2nix . > default.nix
```

#### run app in compose

```
docker-compose up
```
