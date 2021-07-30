module Uniswap.Data.LiquidityPool where

import Prelude
import Uniswap.Data.Coin (Coin)
import Uniswap.Data.Fee (Fee)

-- | FIXME: Add amountA and amountB...
type LiquidityPool
  = { coinA :: Coin
    , coinB :: Coin
    , fee :: Fee
    }
