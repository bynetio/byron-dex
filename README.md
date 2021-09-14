# dex

## Development setup

After cloning the repository, `cd` into the project and do the following:

##### Local Hoogle documentation

In order to run local hoogle with all project dependencies run:

```bash
nix-shell shell-hoogle.nix --run "hoogle server --local"
```

This will open a local Hoogle server at `http://127.0.0.1:8080`

##### Generate ctags

```bash
find vendor/plutus/ -name "*.*hs" \
  | grep -v 'fomega/app/Main.hs' \
  | grep -v 'nofib/src/Plutus/Benchmark' \
  | xargs  hs-tags --cabal uniswap/uniswap.cabal -c
```

### vscode integration

Plugins

- [vscode-haskell](https://github.com/haskell/vscode-haskell)
- [nix-env-selector](https://github.com/arrterian/nix-env-selector)
- [ctagsx](https://github.com/jtanx/ctagsx)

### vim integration

`vim` integration is already built-in in `default.nix` environment. The `nix-shell` environment uses local `.vimrc` file in ones home directory.
To find some examples of how to setup vim to work with `coc` see [this `.vimrc`](https://github.com/placek/dotfiles/blob/master/share/common/.vimrc#L78).

## Project structure

There are 2 main projects in this repository:

- `uniswap` - this project contains all contract logic, defines all contract endpoints and provides a PAB executable.
- `uniswap-json-api` - this project provides a Proxy API which simplifies HTTP communication with PAB.

## `uniswap`

### Contract Endpoints

All contract endpoints are defined in `uniswap/src/Uniswap/OffChain.hs`.

- `start` -- creates a new script instance.
- `create` -- creates a new _LiquidityPool_. _LiquidityPool_ is identified by pair of tokens and fee rate.
- `add` -- adds liquidity to a existing _LiquidityPool_.
- `remove` -- removes liquidity from a _LiquidityPool_.
- `close` -- removes all liquidity from a _LiquidityPool_ and deletes it. This endpoint can only be used when all liquidity in this pool belongs to the person who wants to close it.
- `swap` -- swaps one token for another by using specific _LiquidityPool_.
- `pools` -- provides information about existing _LiquidityPools_.
- `funds` -- provides information about funds belonging to the user.

- `swapPreview` -- checks how much of desired token user would get if he tried to perform swap.
- `iSwapPreview` -- checks how much of desired token user would get if he tried to perform indirect swap.
- `iSwap` -- swaps one token for another by using chain of _LiquidityPools_. Automatically finds the best _swap path_ and performs all intermediate swaps in one transaction.

### Slippage

State of _LiquidityPools_ can change at any time -- even during execution of Off-chain methods. This could be a problem, because user couldn't be sure of how much will he get by performing swap. To solve this problem we allow user to specify expected amount of desired coin and slippage tolerance. Transaction will be submitted only if amount of token returned to user isn't lower than expected value (with slippage tolerance taken to account).

### Custom fees

Original Uniswap implementation had fixed fee rate (0,3%). We allow liquidity providers to chose fee rate that suits them the best.

Fee rate is defined as pair of nominator and denominator. For example the 0.3% fee rate could be represented as (3,1000).

### Indirect Swaps

Indirect swaps comes handy when we want to swap between two tokens and there is not a single _LiquidityPool_ with these two tokens. Indirect swap will check if by performing multiple swaps on diferent _LiquidityPools_ user can get their desired token. All these swaps are performed simultaneously in single transaction.

Currently swap path can include each _LiquidityPool_ at most once. Lets say that there are currently 5 _LiquidityPools_:

- Pool 1 (_A_ 100, _B_ 100), fee rate (3,1000)
- Pool 2 (_B_ 200, _C_ 300), fee rate (3,1000)
- Pool 3 (_D_ 2, _E_ 1_000_000), fee rate (3,1000)
- Pool 4 (_D_ 1*000_000, \_E* 2), fee rate (2,1000)
- Pool 5 (_B_ 100, _D_ 100), fee rate (3,1000)
  If someone wants to swap some amount of token _A_ for token _C_ then indirect swap will just perform 2 swaps (on Pool 1 and Pool 2). In this case someone could take advantage of Pool 3 and Pool 4 having drastically different exchange rates between tokens _D_ and _E_. If the swap path would look like this:

1. Swap _A_ for _B_ in Pool 1
2. Swap _B_ for _D_ in Pool 5
3. Swap _D_ for _E_ in Pool 3
4. Swap _E_ for _D_ in Pool 4
5. Swap _D_ for _B_ in Pool 5
6. Swap _B_ for _C_ in Pool 2

then someone could get much more of token _C_ than if swapping with just Pool 1 and Pool 2. This would require swapping on Pool 5 twice which is currently not supported.

## `uniswap-json-api`

### Strategy

Since the PAB JSON API does not provide a status of the endpoint that has been triggered nor it's return values, we tried to setup some status lookup for API to provide a verbose versions of PAB endpoints for `uniswap` application.

The logic behind that is to trigger a `status` endpoint after each successful endpoint trigger to fetch the status of the endpoint execution.

In order to reach the information we need to ensure we have a success on the endpoint (`200` HTTP code) and a result with corresponding tag for that endpoint.

### API documentation

#### `POST /:id/create`

Creates a new _LiquidityPool_. _LiquidityPool_ is identified by pair of tokens and fee rate.

| name       | type    | data type | description                          |
| ---------- | ------- | --------- | ------------------------------------ |
| `id`       | capture | `Text`    | An instance ID of a wallet.          |
| `coin_a`   | query   | `Text`    | A name of first token in a pair.     |
| `coin_b`   | query   | `Text`    | A name of second token in a pair.    |
| `amount_a` | query   | `Int`     | An amount of first token in a pair.  |
| `amount_a` | query   | `Int`     | An amount of second token in a pair. |

- Returns `200` status code when operation has been triggered properly for a wallet.
- Returns `422` status code if there were a problem with HTTP connection.
- Returns `400` status code when any od the params are invalid.
- Returns status of the operation in body.

#### `POST /:id/swap`

Swaps one token for another by using specific _LiquidityPool_.

| name       | type    | data type | description                          |
| ---------- | ------- | --------- | ------------------------------------ |
| `id`       | capture | `Text`    | An instance ID of a wallet.          |
| `coin_a`   | query   | `Text`    | A name of first token in a pair.     |
| `coin_b`   | query   | `Text`    | A name of second token in a pair.    |
| `amount_a` | query   | `Int`     | An amount of first token in a pair.  |
| `amount_a` | query   | `Int`     | An amount of second token in a pair. |
| `slippage` | query   | `Int`     | A slippage for `swap` operation.     |

- Returns `200` status code when operation has been triggered properly for a wallet.
- Returns `422` status code if there were a problem with HTTP connection.
- Returns `400` status code when any od the params are invalid.
- Returns status of the operation in body.

#### `POST /:id/indirect_swap`

Swaps one token for another by using chain of _LiquidityPools_. Automatically finds the best _swap path_ and performs all intermediate swaps in one transaction.

| name       | type    | data type | description                          |
| ---------- | ------- | --------- | ------------------------------------ |
| `id`       | capture | `Text`    | An instance ID of a wallet.          |
| `coin_a`   | query   | `Text`    | A name of first token in a pair.     |
| `coin_b`   | query   | `Text`    | A name of second token in a pair.    |
| `amount_a` | query   | `Int`     | An amount of first token in a pair.  |
| `amount_a` | query   | `Int`     | An amount of second token in a pair. |
| `slippage` | query   | `Int`     | A slippage for `swap` operation.     |

- Returns `200` status code when operation has been triggered properly for a wallet.
- Returns `422` status code if there were a problem with HTTP connection.
- Returns `400` status code when any od the params are invalid.
- Returns status of the operation in body.

#### `POST /:id/add`

Adds liquidity to a existing _LiquidityPool_.

| name       | type    | data type | description                          |
| ---------- | ------- | --------- | ------------------------------------ |
| `id`       | capture | `Text`    | An instance ID of a wallet.          |
| `coin_a`   | query   | `Text`    | A name of first token in a pair.     |
| `coin_b`   | query   | `Text`    | A name of second token in a pair.    |
| `amount_a` | query   | `Int`     | An amount of first token in a pair.  |
| `amount_a` | query   | `Int`     | An amount of second token in a pair. |

- Returns `200` status code when operation has been triggered properly for a wallet.
- Returns `422` status code if there were a problem with HTTP connection.
- Returns `400` status code when any od the params are invalid.
- Returns status of the operation in body.

#### `POST /:id/remove`

Removes liquidity from a _LiquidityPool_.

| name     | type    | data type | description                       |
| -------- | ------- | --------- | --------------------------------- |
| `id`     | capture | `Text`    | An instance ID of a wallet.       |
| `coin_a` | query   | `Text`    | A name of first token in a pair.  |
| `coin_b` | query   | `Text`    | A name of second token in a pair. |
| `amount` | query   | `Int`     | An amount of liquidity tokens.    |

- Returns `200` status code when operation has been triggered properly for a wallet.
- Returns `422` status code if there were a problem with HTTP connection.
- Returns `400` status code when any od the params are invalid.
- Returns status of the operation in body.

#### `POST /:id/swap_preview`

" :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> QueryParam "amount" Int :> Post '[JSON] ()

#### `POST /:id/indirect_swap_preview`

" :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> QueryParam "amount" Int :> Post '[JSON] ()

#### `POST /:id/close`

Removes all liquidity from a _LiquidityPool_ and deletes it. This endpoint can only be used when all liquidity in this pool belongs to the person who wants to close it.

| name     | type    | data type | description                       |
| -------- | ------- | --------- | --------------------------------- |
| `id`     | capture | `Text`    | An instance ID of a wallet.       |
| `coin_a` | query   | `Text`    | A name of first token in a pair.  |
| `coin_b` | query   | `Text`    | A name of second token in a pair. |

- Returns `200` status code when operation has been triggered properly for a wallet.
- Returns `422` status code if there were a problem with HTTP connection.
- Returns `400` status code when any od the params are invalid.
- Returns status of the operation in body.

#### `GET /:id/pools`

Provides information about existing _LiquidityPools_.

| name | type    | data type | description                 |
| ---- | ------- | --------- | --------------------------- |
| `id` | capture | `Text`    | An instance ID of a wallet. |

- Returns `200` status code when operation has been triggered properly for a wallet.
- Returns `422` status code if there were a problem with HTTP connection.
- Returns `400` status code when any od the params are invalid.
- Returns status of the operation in body.

#### `POST /:id/funds`

Provides information about funds belonging to the user.

| name | type    | data type | description                 |
| ---- | ------- | --------- | --------------------------- |
| `id` | capture | `Text`    | An instance ID of a wallet. |

- Returns `200` status code when operation has been triggered properly for a wallet.
- Returns `422` status code if there were a problem with HTTP connection.
- Returns `400` status code when any od the params are invalid.
- Returns status of the operation in body.

#### `POST /:id/status`

Provides a status of recent operation on wallet.

| name | type    | data type | description                 |
| ---- | ------- | --------- | --------------------------- |
| `id` | capture | `Text`    | An instance ID of a wallet. |

- Returns `200` status code when operation has been triggered properly for a wallet.
- Returns `422` status code if there were a problem with HTTP connection.
- Returns `400` status code when any od the params are invalid.
- Returns status of the operation in body.

## Deployment on docker

The `uniswap-json-api` application has it's own environment settings established with Nix.
The Nix configuration allows to build whole project and compile it using `callCabal2nix` function,
and further load it into a docker image using `dockerTools`.

In order to create a docker image with compiled executables `cd` into `uniswap-json-api` directory and type:

```
docker load < $(nix-build --no-out-link nix/image.nix)
```

The docker image so far weights ~3GB - we need to optimize that to provide minimal image with only an executable and neccesary libaries - not a whole development environment.

The application itself has now hardcoded configuration settings but we aim to provide a ENV-based configuration in order to run an application with any needed configuration, like `uniswap` instance domain/port, etc.

## Development

For development purposes the `uniswap-json-api` projects has a nix-shell setup ready:

```
nix-shell nix/shell.nix
```

One can only also trigger a docker-compose configuration if the docker image is built.

```
docker-compose up
```

## CI

This project use GitLab as CI. Check the configuration [file](./.gitlab-ci.yaml) for more information.
GitLab use custom docker image (`silquenarmo/dex-plutus-dev`) which cointains neccesary dependencies.
The image is build using [devcontainer](./nix/devcontainer/uniswap-devcontainer.nix).
To update it or rebuild manually:

1. `docker load < $(nix-build default.nix -A devcontainer)`.
2. tag result image `docker tag uniswap-devcontainer silquenarmo/dex-plutus-dev:<version>`
3. _[optional]_ push an image to hub repository `docker push silquenarmo/dex-plutus-dev:<version>` and `docker push silquenarmo/dex-plutus-dev:latest`

NOTE: You can build docker image only on linux.

CI runs of external gitlab CI runner on AWS instance, and process is as follows:

- create _r4.large_ instances (2vcpu, 16G ram)
- there are max 2 instances for max two jobs at runtime
- after processing job instances are waiting 1h
- after 1h waiting the instance is being removed
- if next job arises withing a 1h delay it's being processed by a free instance
- the instance creation is about 5min long
- disk capacity per instance is ~40G

CI consists of three stages:

- **lint** - using `hlint` to verify both projects
- **build** - using `cabal build` to compile projects
- **test** - using `cabal test` to run specs on projects

All three stages are being runned in `on_success` mode when they are triggered from default (`master`) branch. On any other branches the pipeline will not be triggered UNLESS there is a merge request created on that branch. The `build` and `test` stages can only be triggered manually and only linting is performed automatically.

## Remote development

The docker image used by CI can be use to remote development with VSCode.

Usage:

1. Create `.devcontainer/devcontainer.json` in your project as below
2. Install the Remote Development extension pack in VSCode
3. Open the folder "in the container"

```json
{
  "name": "Uniswap Project",
  "image": "silquenarmo/dex-plutus-dev:latest",

  // Use 'settings' to set *default* container specific settings.json values on container create.
  // You can edit these settings after create using File > Preferences > Settings > Remote.
  "settings": {
    "terminal.integrated.shell.linux": "/bin/bash"
  },

  "extensions": ["haskell.haskell"]
}
```

## Launching dApp

```bash
$ cabal run uniswap-pab
$ cabal run exe:uniswap-json-api
$ cd uniswap-ui && npm run serve-dev
```
