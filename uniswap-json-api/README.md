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

#### Example CURL requests
curl -XPOST -H 'Content-Type: application/json' --data-binary '{"coinA": "BTC", "coinB": "ETH", "fee": 20, "amountA": 1000, "amountB": 20000}'  localhost:3000/b5a22d53-80d1-4e53-bb56-b32dd559b991/create
