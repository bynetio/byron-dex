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


## Project structure
There are 2 main projects in this repository:
* ```uniswap``` - this project contains all contract logic, defines all contract endpoints and provides a PAB executable.
* ```uniswap-json-api``` - this project provides a Proxy API which simplifies HTTP communication with PAB.


# Contract Endpoints
All contract endpoints are defined in ```uniswap/src/Uniswap/OffChain.hs```.
- ```start``` -- creates a new script instance.
- ```create``` -- creates a new *LiquidityPool*. *LiquidityPool* is identified by pair of tokens and fee rate.
- ```add``` -- adds liquidity to a existing *LiquidityPool*.
- ```remove``` -- removes liquidity from a *LiquidityPool*. 
- ```close``` -- removes all liquidity from a *LiquidityPool* and deletes it. This endpoint can only be used when all liquidity in this pool belongs to the person who wants to close it.
- ```swap``` -- swaps one token for another by using specific *LiquidityPool*.
- ```pools``` -- provides information about existing *LiquidityPools*.
- ```funds``` -- provides information about funds belonging to the user.

- ```swapPreview``` -- checks how much of desired token user would get if he tried to perform swap. 
- ```iSwapPreview``` -- checks how much of desired token user would get if he tried to perform indirect swap.
- ```iSwap``` -- swaps one token for another by using chain of *LiquidityPools*. Automatically finds the best *swap path* and performs all intermediate swaps in one transaction.




## Slippage
State of *LiquidityPools* can change at any time -- even during execution of Off-chain methods. This could be a problem, because user couldn't be sure of how much will he get by performing swap. To solve this problem we allow user to specify expected amount of desired coin and slippage tolerance. Transaction will be submitted only if amount of token returned to user isn't lower than expected value (with slippage tolerance taken to account).


## Custom fees
Original Uniswap implementation had fixed fee rate (0,3%). We allow liquidity providers to chose fee rate that suits them the best. 

Fee rate is defined as pair of nominator and denominator. For example the 0.3% fee rate could be represented as (3,1000). 

## Indirect Swaps
Indirect swaps comes handy when we want to swap between two tokens and there is not a single *LiquidityPool* with these two tokens. Indirect swap will check if by performing multiple swaps on diferent *LiquidityPools* user can get their desired token. All these swaps are performed simultaneously in single transaction.

Currently swap path can include each *LiquidityPool* at most once. Lets say that there are currently 5 *LiquidityPools*:
* Pool 1 (*A* 100, *B* 100), fee rate (3,1000)
* Pool 2 (*B* 200, *C* 300), fee rate (3,1000)
* Pool 3 (*D* 2, *E* 1_000_000), fee rate (3,1000)
* Pool 4 (*D* 1_000_000, *E* 2), fee rate (2,1000)
* Pool 5 (*B* 100, *D* 100), fee rate (3,1000)
If someone wants to swap some amount of token *A* for token *C* then indirect swap will just perform 2 swaps (on Pool 1 and Pool 2). In this case someone could take advantage of Pool 3 and Pool 4 having drastically different exchange rates between tokens *D* and *E*. If the swap path would look like this:

1. Swap *A* for *B* in Pool 1
2. Swap *B* for *D* in Pool 5
3. Swap *D* for *E* in Pool 3
4. Swap *E* for *D* in Pool 4
5. Swap *D* for *B* in Pool 5
6. Swap *B* for *C* in Pool 2

then someone could get much more of token *C* than if swapping with just Pool 1 and Pool 2. This would require swapping on Pool 5 twice which is currently not supported.
  
