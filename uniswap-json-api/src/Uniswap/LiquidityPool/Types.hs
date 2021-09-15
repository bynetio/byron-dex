{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Uniswap.LiquidityPool.Types
  where

import Data.Char            (toLower)
import Data.Text            (Text)
import Deriving.Aeson       (CustomJSON (CustomJSON), FromJSON, Generic, ToJSON)
import Uniswap.Common.Utils (PrefixedCamelCase)
import Uniswap.PAB.Types    (Coin, Fee)

data CreatePoolForm
  = CreatePoolForm
      { -- | One 'Coin' of the liquidity pair.
        coinA   :: Coin
        -- | The other 'Coin'.
      , coinB   :: Coin
        -- | Numerator and denominator of the swap fee
      , fee     :: Fee
        -- | Amount of liquidity for the first 'Coin'.
      , amountA :: Integer
        -- | Amount of liquidity for the second 'Coin'.
      , amountB :: Integer
      }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON)

data CloseForm
  = CloseForm
      { -- | One 'Coin' of the liquidity pair.
        coinA :: Coin
        -- | The other 'Coin' of the liquidity pair.
      , coinB :: Coin
        -- | Numerator and denominator of the swap fee
      , fee   :: Fee
      }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON)

data SwapForm
  = SwapForm
      { -- | One 'Coin' of the liquidity pair.
        coinA    :: Coin
        -- | The other 'Coin'.
      , coinB    :: Coin
        -- | Numerator and denominator of the swap fee
      , fee      :: Fee
        -- | The amount the first 'Coin' that should be swapped.
      , amount   :: Integer
        -- | The expected amount of swaped 'Text' (quoted amount)
      , result   :: Integer
        -- | The expected % difference between quoted and executed prices.
      , slippage :: Integer
      }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON)

data SwapPreviewForm
  = SwapPreviewForm
      { coinA  :: Coin
        -- | Numerator and denominator of the swap fee
      , coinB  :: Coin
      , fee    :: Fee
      , amount :: Integer
      }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON)

data IndirectSwapForm
  = IndirectSwapForm
      { -- | One 'Coin' of the liquidity pair.
        coinA    :: Coin
        -- | The other 'Coin'.
      , coinB    :: Coin
        -- | The amount of the first 'Coin' that should be swapped.
      , amount   :: Integer
      , result   :: Integer
      , slippage :: Integer
      }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON)

data ISwapPreviewForm
  = ISwapPreviewForm
      { coinA  :: Coin
      , coinB  :: Coin
      , amount :: Integer
      }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON)

data RemoveForm
  = RemoveForm
      { -- | One 'Coin' of the liquidity pair.
        coinA :: Coin
        -- | The other 'Coin' of the liquidity pair.
      , coinB :: Coin
        -- | Numerator and denominator of the swap fee
      , fee   :: Fee
        -- | The amount of liquidity tokens to burn in exchange for liquidity from the pool.
      , diff  :: Integer
      }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON)

data AddForm
  = AddForm
      { -- | One 'Coin' of the liquidity pair.
        coinA   :: Coin
        -- | The other 'Coin' of the liquidity pair.
      , coinB   :: Coin
        -- | Numerator and denominator of the swap fee
      , fee     :: Fee
        -- | The amount of coins of the first kind to add to the pool.
      , amountA :: Integer
        -- | The amount of coins of the second kind to add to the pool.
      , amountB :: Integer
      }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON)

data LiquidityPoolWithCoins
  = LiquidityPoolWithCoins
      { coinA         :: Coin
      , coinB         :: Coin
      , fee           :: Fee
      , amountA       :: Integer
      , amountB       :: Integer
      , liquidityCoin :: Coin
      }
  deriving (FromJSON, Generic, Show, ToJSON)

data AmountOfCoin
  = AmountOfCoin
      { coin   :: Coin
      , amount :: Integer
      }
  deriving (FromJSON, Generic, Show, ToJSON)
