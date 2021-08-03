module Uniswap.Data.Fee where

import Prelude
import Data.Codec ((~))
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Int (toNumber)

type Fee
  = { numerator :: Int
    , denominator :: Int
    }

toString :: Fee -> String
toString { numerator, denominator } = show ((toNumber numerator `div` toNumber denominator) * toNumber 100) <> " %"

codec :: JsonCodec Fee
codec =
  CA.indexedArray "Fee"
    $ { numerator: _, denominator: _ }
    <$> _.numerator
    ~ CA.index 0 CA.int
    <*> _.denominator
    ~ CA.index 1 CA.int
