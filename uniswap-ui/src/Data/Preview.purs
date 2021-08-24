module Uniswap.Data.Preview where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Record as CAR
import Uniswap.Data.Amount (Amount)
import Uniswap.Data.Amount as Amount
import Uniswap.Data.Coin (Coin)
import Uniswap.Data.Coin as Coin
import Uniswap.Data.Fee (Fee)
import Uniswap.Data.Fee as Fee

type PreviewRep row
  = ( coinA :: Coin
    , amountA :: Amount
    , coinB :: Coin
    , amountB :: Amount
    | row
    )

type SwapPreview
  = { | PreviewRep () }

type ISwapPreview
  = { | PreviewRep ( fee :: Fee ) }

swapPreviewCodec :: JsonCodec SwapPreview
swapPreviewCodec =
  CAR.object "SwapPreview"
    { coinA: Coin.codec
    , amountA: Amount.codec
    , coinB: Coin.codec
    , amountB: Amount.codec
    }

iSwapPreviewCodec :: JsonCodec ISwapPreview
iSwapPreviewCodec =
  CAR.object "ISwapPreview"
    { coinA: Coin.codec
    , amountA: Amount.codec
    , coinB: Coin.codec
    , amountB: Amount.codec
    , fee: Fee.codec
    }
