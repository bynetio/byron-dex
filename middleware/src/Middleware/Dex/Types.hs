{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |

module Middleware.Dex.Types where

import Data.Aeson.Types (ToJSON)
import Data.Text        (Text)
import GHC.Generics     (Generic)
import Ledger           (CurrencySymbol, TokenName)

newtype Error = Error
  { errorMessage :: Text
  } deriving (Generic, ToJSON)

-- | Data type to represent wallet founds.
data FundView = FundView
  { coin   :: Coin
  , amount :: Integer
  } deriving (Show, Generic, ToJSON)

data Coin = Coin
  { currencySymbol :: CurrencySymbol
  , tokenName      :: TokenName
  } deriving (Show, Generic, ToJSON)
