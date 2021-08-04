module Uniswap.Data.Amount where

import Prelude

import Data.Argonaut.Core as J
import Data.BigInt (BigInt)
import Data.BigInt as BI
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)

newtype Amount
  = Amount BigInt

derive instance newtypeAmount :: Newtype Amount _

derive instance eqAmount :: Eq Amount

derive instance ordAmount :: Ord Amount

codec :: JsonCodec Amount
codec = wrapIso Amount bigint

toString :: Amount -> String
toString (Amount a) = BI.toString a

-- | A codec for `BigInt` values in `Json`.
bigint ∷ JsonCodec BigInt
bigint = CA.prismaticCodec "BigInt" BI.fromNumber BI.toNumber CA.number
