{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Uniswap.PAB.Types
  where

import Data.Aeson           (FromJSON, ToJSON)
import Data.Text            (Text)
import GHC.Generics         (Generic)
import UniswapJsonApi.Types (OperationId)

type Fee = Integer

data WithHistoryId a = WithHistoryId
  { historyId :: OperationId
  , content   :: a
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @create@-endpoint, which creates a new liquidity pool.
data CreateParams = CreateParams
  { cpCoinA     :: Text
    -- ^ The other 'Coin'.
    , cpCoinB   :: Text
    -- ^ One 'Coin' of the liquidity pair.
    , cpFee     :: Fee
    -- ^ Numerator and denominator of the swap fee
    , cpAmountA :: Integer
    -- ^ Amount of liquidity for the first 'Coin'.
    , cpAmountB :: Integer
    -- ^ Amount of liquidity for the second 'Coin'.
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @swap@-endpoint, which allows swaps between the two different coins in a liquidity pool.
-- One of the provided amounts must be positive, the other must be zero.
data SwapParams = SwapParams
  { spCoinA    :: Text
  -- ^ One 'Coin' of the liquidity pair.
  , spCoinB    :: Text
  -- ^ The other 'Coin'.
  , spFee      :: Fee
  -- ^ Numerator and denominator of the swap fee
  , spAmount   :: Integer
  -- ^ The amount the first 'Coin' that should be swapped.
  , spResult   :: Integer
  -- ^ The expected amount of swaped 'Text' (quoted amount)
  , spSlippage :: Integer
  -- ^ The expected % difference between quoted and executed prices.
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data SwapPreviewParams = SwapPreviewParams
  { sppCoinA   :: Text
  ,  sppCoinB  :: Text
  ,  sppFee    :: Fee
  -- ^ Numerator and denominator of the swap fee
  ,  sppAmount :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data IndirectSwapParams = IndirectSwapParams
  { ispCoinA    :: Text
  -- ^ One 'Coin' of the liquidity pair.
  , ispCoinB    :: Text
    -- ^ The other 'Coin'.
  , ispAmount   :: Integer
    -- ^ The amount of the first 'Coin' that should be swapped.
  , ispResult   :: Integer
  , ispSlippage :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ISwapPreviewParams = ISwapPreviewParams
  { isppCoinA  :: Text
  , isppCoinB  :: Text
  , isppAmount :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @close@-endpoint, which closes a liquidity pool.
data CloseParams = CloseParams
  { clpCoinA :: Text
  -- ^ One 'Coin' of the liquidity pair.
  , clpCoinB :: Text
  -- ^ The other 'Coin' of the liquidity pair.
  , clpFee   :: Fee
  -- ^ Numerator and denominator of the swap fee
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @remove@-endpoint, which removes some liquidity from a liquidity pool.
data RemoveParams = RemoveParams
  { rpCoinA :: Text
  -- ^ One 'Coin' of the liquidity pair.
  , rpCoinB :: Text
  -- ^ The other 'Coin' of the liquidity pair.
  , rpFee   :: Fee
  -- ^ Numerator and denominator of the swap fee
  , rpDiff  :: Integer
  -- ^ The amount of liquidity tokens to burn in exchange for liquidity from the pool.
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @add@-endpoint, which adds liquidity to a liquidity pool in exchange for liquidity tokens.
data AddParams = AddParams
  { apCoinA   :: Text
  -- ^ One 'Coin' of the liquidity pair.
  , apCoinB   :: Text
  -- ^ The other 'Coin' of the liquidity pair.
  , apFee     :: Fee
  -- ^ Numerator and denominator of the swap fee
  , apAmountA :: Integer
  -- ^ The amount of coins of the first kind to add to the pool.
  , apAmountB :: Integer
  -- ^ The amount of coins of the second kind to add to the pool.
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @clearState-@endpoint, which removes entry from the state corresponding to given OperationId
data ClearStateParams = ClearStateParams
  { clsRemoveId :: OperationId
    -- ^ Identifier of Operation that should be removed from state
  }
  deriving (Show, Generic, ToJSON, FromJSON)
