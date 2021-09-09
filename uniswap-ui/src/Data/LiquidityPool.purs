module Uniswap.Data.LiquidityPool where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut as CA
import Uniswap.Data.Coin (Coin)
import Uniswap.Data.Coin as Coin
import Uniswap.Data.Fee (Fee)
import Uniswap.Data.Fee as Fee
import Uniswap.Data.Amount (Amount)
import Uniswap.Data.Amount as Amount

type LiquidityPoolRep row
  = ( coinA :: Coin
    , coinB :: Coin
    , fee :: Fee
    | row
    )

type Amounts
  = ( amountA :: Amount
    , amountB :: Amount
    )

type LiquidityPool
  = { | LiquidityPoolRep Amounts }

type RemoveLiquidityPool
  = { | LiquidityPoolRep ( diff :: Int ) }

type CloseLiquidityPool
  = { | LiquidityPoolRep () }

codecLiquidityPool :: JsonCodec LiquidityPool
codecLiquidityPool =
  CAR.object "LiquidityPool"
    { coinA: Coin.codec
    , amountA: Amount.codec
    , coinB: Coin.codec
    , amountB: Amount.codec
    , fee: Fee.codec
    }

codecRemoveLiquidityPool :: JsonCodec RemoveLiquidityPool
codecRemoveLiquidityPool =
  CAR.object "RemoveLiquidityPool"
    { coinA: Coin.codec
    , coinB: Coin.codec
    , fee: Fee.codec
    , diff: CA.int
    }

codecCloseLiquidityPool :: JsonCodec CloseLiquidityPool
codecCloseLiquidityPool =
  CAR.object "CloseLiquidityPool"
    { coinA: Coin.codec
    , coinB: Coin.codec
    , fee: Fee.codec
    }
