{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Uniswap.PAB.Types
  where

import Data.Aeson           (FromJSON, ToJSON)
import Data.Text            (Text)
import GHC.Generics         (Generic)
import UniswapJsonApi.Types (OperationId)

type Fee = Integer


-- | Parameters for the @create@-endpoint, which creates a new liquidity pool.
data CreateParams = CreateParams
  { -- | Unique Identifier of Operation
    cpOpId    :: OperationId,
    -- | One 'Coin' of the liquidity pair.
    cpCoinA   :: Text,
    -- | The other 'Coin'.
    cpCoinB   :: Text,
    -- | Numerator and denominator of the swap fee
    cpFee     :: Fee,
    -- | Amount of liquidity for the first 'Coin'.
    cpAmountA :: Integer,
    -- | Amount of liquidity for the second 'Coin'.
    cpAmountB :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @swap@-endpoint, which allows swaps between the two different coins in a liquidity pool.
-- One of the provided amounts must be positive, the other must be zero.
data SwapParams = SwapParams
  { -- | Unique Identifier of Operation
    spOpId     :: OperationId,
    -- | One 'Coin' of the liquidity pair.
    spCoinA    :: Text,
    -- | The other 'Coin'.
    spCoinB    :: Text,
    -- | Numerator and denominator of the swap fee
    spFee      :: Fee,
    -- | The amount the first 'Coin' that should be swapped.
    spAmount   :: Integer,
    -- | The expected amount of swaped 'Text' (quoted amount)
    spResult   :: Integer,
    -- | The expected % difference between quoted and executed prices.
    spSlippage :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data SwapPreviewParams = SwapPreviewParams
  { -- | Unique Identifier of Operation
    sppOpId   :: OperationId,
    sppCoinA  :: Text,
    sppCoinB  :: Text,
    -- | Numerator and denominator of the swap fee
    sppFee    :: Fee,
    sppAmount :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data IndirectSwapParams = IndirectSwapParams
  { -- | Unique Identifier of Operation
    ispOpId     :: OperationId,
    -- | One 'Coin' of the liquidity pair.
    ispCoinA    :: Text,
    -- | The other 'Coin'.
    ispCoinB    :: Text,
    -- | The amount of the first 'Coin' that should be swapped.
    ispAmount   :: Integer,
    ispResult   :: Integer,
    ispSlippage :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ISwapPreviewParams = ISwapPreviewParams
  { -- | Unique Identifier of Operation
    isppOpId   :: OperationId,
    isppCoinA  :: Text,
    isppCoinB  :: Text,
    isppAmount :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @close@-endpoint, which closes a liquidity pool.
data CloseParams = CloseParams
  { -- | Unique Identifier of Operation
    clpOpId  :: OperationId,
    -- | One 'Coin' of the liquidity pair.
    clpCoinA :: Text,
    -- | The other 'Coin' of the liquidity pair.
    clpCoinB :: Text,
    -- | Numerator and denominator of the swap fee
    clpFee   :: Fee
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @remove@-endpoint, which removes some liquidity from a liquidity pool.
data RemoveParams = RemoveParams
  { -- | Unique Identifier of Operation
    rpOpId  :: OperationId,
    -- | One 'Coin' of the liquidity pair.
    rpCoinA :: Text,
    -- | The other 'Coin' of the liquidity pair.
    rpCoinB :: Text,
    -- | Numerator and denominator of the swap fee
    rpFee   :: Fee,
    -- | The amount of liquidity tokens to burn in exchange for liquidity from the pool.
    rpDiff  :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @add@-endpoint, which adds liquidity to a liquidity pool in exchange for liquidity tokens.
data AddParams = AddParams
  { -- | Unique Identifier of Operation
    apOpId    :: OperationId,
    -- | One 'Coin' of the liquidity pair.
    apCoinA   :: Text,
    -- | The other 'Coin' of the liquidity pair.
    apCoinB   :: Text,
    -- | Numerator and denominator of the swap fee
    apFee     :: Fee,
    -- | The amount of coins of the first kind to add to the pool.
    apAmountA :: Integer,
    -- | The amount of coins of the second kind to add to the pool.
    apAmountB :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @pools-@endpoint, which lookups available pools
data PoolsParams = PoolsParams
  { -- | Unique Identifier of Operation
    plOpId :: OperationId
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @funds-@endpoint, which lookups funds of the current wallet
data FundsParams = FundsParams
  { -- | Unique Identifier of Operation
    fsOpId :: OperationId
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @stop-@endpoint, which stops the uniswap instance
data StopParams = StopParams
  { -- | Unique Identifier of Operation
    stOpId :: OperationId
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @clearState-@endpoint, which removes entry from the state corresponding to given OperationId
data ClearStateParams = ClearStateParams
  { -- | Unique Identifier of Operation
    clsOpId     :: OperationId,
    -- | Identifier of Operation that should be removed from state
    clsRemoveId :: OperationId
  }
  deriving (Show, Generic, ToJSON, FromJSON)
