module Uniswap.Data.Coin where

import Prelude
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)

newtype TokenName
  = TokenName String

derive instance newtypeTokenName :: Newtype TokenName _

derive instance eqTokenName :: Eq TokenName

derive instance ordTokenName :: Ord TokenName

instance showTokenName :: Show TokenName where
  show (TokenName name) = name

newtype CurrencySymbol
  = CurrencySymbol String

derive instance newtypeCurrencySymbol :: Newtype CurrencySymbol _

derive instance eqCurrencySymbol :: Eq CurrencySymbol

derive instance ordCurrencySymbol :: Ord CurrencySymbol

instance showCurrencySymbol :: Show CurrencySymbol where
  show (CurrencySymbol symbol) = symbol

type Coin
  = { currencySymbol :: CurrencySymbol
    , tokenName :: TokenName
    }

codec :: JsonCodec Coin
codec =
  CAR.object "Coin"
    { currencySymbol: wrapIso CurrencySymbol CA.string
    , tokenName: wrapIso TokenName CA.string
    }
