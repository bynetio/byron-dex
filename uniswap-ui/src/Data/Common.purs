module Uniswap.Data.Common (bigintCodec) where

import Data.BigInt as BI
import Data.BigInt (BigInt)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA

-- | A codec for `BigInt` values in `Json`.
bigintCodec ∷ JsonCodec BigInt
bigintCodec = CA.prismaticCodec "BigInt" BI.fromNumber BI.toNumber CA.number
