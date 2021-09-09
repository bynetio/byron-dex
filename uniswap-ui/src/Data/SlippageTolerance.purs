module Uniswap.Data.SlippageTolerance where

import Prelude
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Codec.Argonaut (JsonCodec)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Uniswap.Data.Common (bigintCodec)

newtype SlippageTolerance
  = SlippageTolerance BigInt

derive instance newtypeTokenName :: Newtype SlippageTolerance _

derive instance eqTokenName :: Eq SlippageTolerance

derive instance ordTokenName :: Ord SlippageTolerance

instance showSlippageTolerance :: Show SlippageTolerance where
  show (SlippageTolerance t) = show t

ofInt :: Int -> SlippageTolerance
ofInt i = SlippageTolerance $ BigInt.fromInt i

codec :: JsonCodec SlippageTolerance
codec = wrapIso SlippageTolerance bigintCodec
