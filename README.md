# dex

## Development setup

After cloning the repository, `cd` into the project and do the following:

##### Local Hoogle documentation

In order to run local hoogle with all project dependencies run:

```bash
nix-shell shell-hoogle.nix --run "hoogle server --local"
```

This will open a local Hoogle server at `http://127.0.0.1:8080`

### Integrate nix and hie-bios

```bash
export NIX_GHC="$(which ghc)"
export NIX_GHCPKG="$(which ghc-pkg)"
export NIX_GHC_DOCDIR="$NIX_GHC/../../share/doc/ghc/html"
export NIX_GHC_LIBDIR="$(ghc --print-libdir)"
```

### vscode integration

Plugins

- [vscode-haskell](https://github.com/haskell/vscode-haskell)
- [nix-env-selector](https://github.com/arrterian/nix-env-selector)
- [ctagsx](https://github.com/jtanx/ctagsx)

### vim integration

It is recommended to use [ALE](https://github.com/dense-analysis/ale) as a primary lint and hls integration.
However, because of some bug related to `cabal build` being launched from directory containing linted file
and not from project root (vim CWD), use the following configuration:

```vimL
call ale#linter#Define('haskell', {
\  'name':          'my_cabal',
\  'aliases':       ['my-cabal'],
\  'output_stream': 'stderr',
\  'executable':    'cabal',
\  'command':       '%e build -fno-code -v0 -- %s </dev/null',
\  'callback':      'ale#handlers#haskell#HandleGHCFormat',
\})

if !exists('g:ale_linters')
  let g:ale_linters = {}
endif
let g:ale_linters.haskell = ['my-cabal', 'hls', 'hlint']
let g:ale_fixers.haskell = 'stylish-haskell'
let g:ale_haskell_hls_executable = 'haskell-language-server'
```

It's also useful to use [haskell-vim](https://github.com/neovimhaskell/haskell-vim) extension.

## PAB and dex endpoints

### Contract Endpoints

All contract endpoints are defined in `uniswap/src/Uniswap/OffChain.hs`.

- `createSellOrder` -- creates an exchange request, called _Swap Order_, i.e. a will to sell some amount of tokens of one kind for some other amount of tokens of other kind.
- `createLiquidityOrder` -- creates a script, called _Liquidity Order_, that allocates some funds that can be used in exchange by other users. User can set up how much funds of two different tokens they wish to put in such pool and setup a fee.
- `createLiquidityPool`
- `perform` -- matches _Swap Orders_ with other _Swap Orders_ and/or _Liquidity Orders_ in order to perform an exchange between them. The performer pays for the transaction, but can also gain some surplus of tokens, when there is some.
- `stop` -- stops connection between wallet and PAB.
- `funds` -- provides information about funds belonging to the user.
- `allOrders` - provides information about all orders on the script.
- `myOrders` - provides information about all orders on the script that belong to the wallet calling this endpoint.
- `cancel` -- closes the _Liquidity Order_ by getting remaining allocated funds back to _Liquidity Order_ founder.
- `collectFunds` -- a transaction that withdraws the tokens to their new owners after a properly performed swap.

## CI

This project use GitLab as CI. Check the configuration [file](./.gitlab-ci.yaml) for more information.
GitLab use custom docker image (`plutus4binarapps/dex-plutus-dev`) which contains necessary dependencies.
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
- **prebuild** - using `cabal update` and `cabal configure` to setup the build environment
- **build** - using `cabal build` to compile projects

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
cabal test dex-test
cabal run dex-pab
```
