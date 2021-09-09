module Uniswap.Data.Swap where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Record as CAR
import Uniswap.Data.Amount (Amount)
import Uniswap.Data.Amount as Amount
import Uniswap.Data.Coin (Coin)
import Uniswap.Data.Coin as Coin
import Uniswap.Data.Fee (Fee)
import Uniswap.Data.Fee as Fee
import Uniswap.Data.SlippageTolerance (SlippageTolerance)
import Uniswap.Data.SlippageTolerance as SlippageTolerance

type SwapRep row
  = ( coinA :: Coin
    , coinB :: Coin
    , amount :: Amount
    | row
    )

type SwapPreview
  = { | SwapRep ( fee :: Fee ) }

type Swap
  = { | SwapRep ( fee :: Fee, result :: Amount, slippage :: SlippageTolerance ) }

type ISwapPreview
  = { | SwapRep () }

type ISwap
  = { | SwapRep ( result :: Amount, slippage :: SlippageTolerance ) }

type SwapPreviewResponse
  = { coinA :: Coin
    , coinB :: Coin
    , amountA :: Amount
    , amountB :: Amount
    }

indirectOf :: SwapPreviewResponse -> SlippageTolerance -> ISwap
indirectOf preview slippage =
  { coinA: preview.coinA
  , coinB: preview.coinB
  , amount: preview.amountA
  , result: preview.amountB
  , slippage
  }

swapPreviewCodec :: JsonCodec SwapPreview
swapPreviewCodec =
  CAR.object "SwapPreview"
    { coinA: Coin.codec
    , coinB: Coin.codec
    , amount: Amount.codec
    , fee: Fee.codec
    }

swapCodec :: JsonCodec Swap
swapCodec =
  CAR.object "Swap"
    { coinA: Coin.codec
    , coinB: Coin.codec
    , amount: Amount.codec
    , fee: Fee.codec
    , result: Amount.codec
    , slippage: SlippageTolerance.codec
    }

iSwapPreviewCodec :: JsonCodec ISwapPreview
iSwapPreviewCodec =
  CAR.object "ISwapPreview"
    { coinA: Coin.codec
    , coinB: Coin.codec
    , amount: Amount.codec
    }

iSwapCodec :: JsonCodec ISwap
iSwapCodec =
  CAR.object "IndirectSwap"
    { coinA: Coin.codec
    , coinB: Coin.codec
    , amount: Amount.codec
    , result: Amount.codec
    , slippage: SlippageTolerance.codec
    }

swapPreviewResponseCodec :: JsonCodec SwapPreviewResponse
swapPreviewResponseCodec =
  CAR.object "SwapPreviewResponse"
    { coinA: Coin.codec
    , coinB: Coin.codec
    , amountA: Amount.codec
    , amountB: Amount.codec
    }
