module Uniswap.Data.LiquidityPool where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Record as CAR
import Uniswap.Data.Coin (Coin)
import Uniswap.Data.Coin as Coin
import Uniswap.Data.Fee (Fee)
import Uniswap.Data.Fee as Fee
import Uniswap.Data.Amount (Amount)
import Uniswap.Data.Amount as Amount

type LiquidityPool
  = { coinA :: Coin
    , amountA :: Amount
    , coinB :: Coin
    , amountB :: Amount
    , fee :: Fee
    }

codec :: JsonCodec LiquidityPool
codec =
  CAR.object "LiquidityPool"
    { coinA: Coin.codec
    , amountA: Amount.codec
    , coinB: Coin.codec
    , amountB: Amount.codec
    , fee: Fee.codec
    }
