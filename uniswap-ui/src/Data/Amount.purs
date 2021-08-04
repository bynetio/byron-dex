module Uniswap.Data.Amount where

import Prelude
import Data.Codec.Argonaut (JsonCodec)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Codec.Argonaut as CA

newtype Amount
  = Amount Int

derive instance newtypeAmount :: Newtype Amount _

derive instance eqAmount :: Eq Amount

derive instance ordAmount :: Ord Amount

codec :: JsonCodec Amount
codec = wrapIso Amount CA.int

toString :: Amount -> String
toString (Amount a) = show a
