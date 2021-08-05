{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Uniswap.LiquidityPool.Types
  where

import           Data.Char            (toLower)
import           Data.Text            (Text)
import           Deriving.Aeson       (CustomJSON (CustomJSON), FromJSON,
                                       Generic, ToJSON)
import           Uniswap.Common.Utils (PrefixedCamelCase)
import           Uniswap.PAB.Types    (Coin, Fee)

data CreatePoolForm = CreatePoolForm
  { coinA   :: Coin
  -- ^ One 'Coin' of the liquidity pair.
  , coinB   :: Coin
  -- ^ The other 'Coin'.
  , fee     :: Fee
  -- ^ Numerator and denominator of the swap fee
  , amountA :: Integer
  -- ^ Amount of liquidity for the first 'Coin'.
  , amountB :: Integer
  -- ^ Amount of liquidity for the second 'Coin'.
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON)

data CloseForm = CloseForm
  { coinA :: Coin
  -- ^ One 'Coin' of the liquidity pair.
  , coinB :: Coin
  -- ^ The other 'Coin' of the liquidity pair.
  , fee   :: Fee
  -- ^ Numerator and denominator of the swap fee
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON)

data SwapForm = SwapForm
  { coinA    :: Coin
  -- ^ One 'Coin' of the liquidity pair.
  , coinB    :: Coin
  -- ^ The other 'Coin'.
  , fee      :: Fee
  -- ^ Numerator and denominator of the swap fee
  , amount   :: Integer
  -- ^ The amount the first 'Coin' that should be swapped.
  , result   :: Integer
  -- ^ The expected amount of swaped 'Text' (quoted amount)
  , slippage :: Integer
  -- ^ The expected % difference between quoted and executed prices.
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON)

data SwapPreviewForm = SwapPreviewForm
  { coinA  :: Coin
  , coinB  :: Coin
  -- ^ Numerator and denominator of the swap fee
  , fee    :: Fee
  , amount :: Integer
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON)

data IndirectSwapForm = IndirectSwapForm
  { coinA    :: Coin
  -- ^ One 'Coin' of the liquidity pair.
  , coinB    :: Coin
  -- ^ The other 'Coin'.
  , amount   :: Integer
  -- ^ The amount of the first 'Coin' that should be swapped.
  , result   :: Integer
  , slippage :: Integer
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON)

data ISwapPreviewForm = ISwapPreviewForm
  { coinA  :: Coin
  , coinB  :: Coin
  , amount :: Integer
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON)

data RemoveForm = RemoveForm
  { coinA :: Coin
  -- ^ One 'Coin' of the liquidity pair.
  , coinB :: Coin
  -- ^ The other 'Coin' of the liquidity pair.
  , fee   :: Fee
  -- ^ Numerator and denominator of the swap fee
  , diff  :: Integer
  -- ^ The amount of liquidity tokens to burn in exchange for liquidity from the pool.
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON)

data AddForm = AddForm
  { coinA   :: Coin
  -- ^ One 'Coin' of the liquidity pair.
  , coinB   :: Coin
  -- ^ The other 'Coin' of the liquidity pair.
  , fee     :: Fee
  -- ^ Numerator and denominator of the swap fee
  , amountA :: Integer
  -- ^ The amount of coins of the first kind to add to the pool.
  , amountB :: Integer
  -- ^ The amount of coins of the second kind to add to the pool.
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON)


data LiquidityPoolWithCoins = LiquidityPoolWithCoins
  { coinA   :: Coin,
    coinB   :: Coin,
    fee     :: Fee,
    amountA :: Integer,
    amountB :: Integer
  } deriving (Show, Generic, ToJSON, FromJSON)

data AmountOfCoin = AmountOfCoin
  { coin   :: Coin
  , amount :: Integer
  } deriving (Show, Generic, FromJSON, ToJSON)

