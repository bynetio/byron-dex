module Uniswap.Data.Funds where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Record as CAR
import Uniswap.Data.Amount (Amount)
import Uniswap.Data.Amount as Amount
import Uniswap.Data.Coin (Coin)
import Uniswap.Data.Coin as Coin

type Fund
  = { coin :: Coin
    , amount :: Amount
    }

codec :: JsonCodec Fund
codec =
  CAR.object "Fund"
    { coin: Coin.codec
    , amount: Amount.codec
    }
