{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |

module Middleware.Dex.Types where

import           Data.Aeson.Encoding.Internal (tuple, (>*<))
import           Data.Aeson.Types             (FromJSON, ToJSON, toJSON)
import           Data.Text                    (Text)
import           GHC.Generics                 (Generic)
import           Ledger                       (CurrencySymbol, TokenName)
import           Ledger.Value                 (assetClass)

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
  } deriving (Show, Generic, FromJSON)

data CreateLiquidityPoolParams
  = CreateLiquidityPoolParams
      { coinA           :: Coin
      , coinB           :: Coin
      , amountA         :: Integer
      , poolPartsParams :: PoolPartsParams
      , swapFee         :: (Integer, Integer)
      , exchangeRate    :: (Integer, Integer)
      }
  deriving (Generic, FromJSON, ToJSON, Show)

data PoolPartsParams
  = PriceChangeParams
      { coinAPriceChange :: (Integer, Integer)
      , coinBPriceChange :: (Integer, Integer)
      , numberOfParts    :: Integer
      }
  deriving (FromJSON, Generic, Show, ToJSON)

instance ToJSON Coin where
  toJSON (Coin a b) = toJSON $ assetClass a b


-- { "coinA": { "tokenName": { "unTokenName": "A" }, "currencySymbol": { "unCurrencySymbol": "aa906c3a72afdd99d48a001f4c73cbf8cf54c62493e0d00774f32698" } },
--   "coinB": { "tokenName": { "unTokenName": "B" }, "currencySymbol": { "unCurrencySymbol": "aa906c3a72afdd99d48a001f4c73cbf8cf54c62493e0d00774f32698" } },
--   "amountA": 1000,
--   "poolPartsParams": {
--     "coinAPriceChange": [100,101],
--     "coinBPriceChange": [100,101],
--     "numberOfParts": 3
--   },
--   "swapFee": [1,100],
--   "exchangeRate": [100,100]
-- }
