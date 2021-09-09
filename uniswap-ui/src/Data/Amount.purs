module Uniswap.Data.Amount where

import Prelude
import Data.BigInt (BigInt)
import Data.BigInt as BI
import Data.Codec.Argonaut (JsonCodec)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Uniswap.Data.Common (bigintCodec)

newtype Amount
  = Amount BigInt

derive instance newtypeAmount :: Newtype Amount _

derive instance eqAmount :: Eq Amount

derive instance ordAmount :: Ord Amount

codec :: JsonCodec Amount
codec = wrapIso Amount bigintCodec

toString :: Amount -> String
toString (Amount a) = BI.toString a

zero :: Amount
zero = Amount $ BI.fromInt 0
