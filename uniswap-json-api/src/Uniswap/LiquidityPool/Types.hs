{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia   #-}
module Uniswap.LiquidityPool.Types
  where


import Data.Char            (toLower)
import Data.Text            (Text)
import Deriving.Aeson       (CustomJSON (CustomJSON), FromJSON, Generic, ToJSON)
import Uniswap.Common.Utils (PrefixedCamelCase)
import Uniswap.PAB.Types    (Fee)

data CreatePoolForm = CreatePoolForm
  { cpCoinA   :: Text
  -- ^ One 'Coin' of the liquidity pair.
  , cpCoinB   :: Text
  -- ^ The other 'Coin'.
  , cpFee     :: Fee
  -- ^ Numerator and denominator of the swap fee
  , cpAmountA :: Integer
  -- ^ Amount of liquidity for the first 'Coin'.
  , cpAmountB :: Integer
  -- ^ Amount of liquidity for the second 'Coin'.
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON)
    via PrefixedCamelCase "cp" CreatePoolForm

data CloseForm = CloseForm
  { clpCoinA :: Text
  -- ^ One 'Coin' of the liquidity pair.
  , clpCoinB :: Text
  -- ^ The other 'Coin' of the liquidity pair.
  , clpFee   :: Fee
  -- ^ Numerator and denominator of the swap fee
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON)
    via PrefixedCamelCase "clp" CloseForm

data SwapForm = SwapForm
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
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON)
    via PrefixedCamelCase "sp" SwapForm

data SwapPreviewForm = SwapPreviewForm
  { sppCoinA  :: Text
  , sppCoinB  :: Text
  -- ^ Numerator and denominator of the swap fee
  , sppFee    :: Fee
  , sppAmount :: Integer
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON)
    via PrefixedCamelCase "spp" SwapPreviewForm

data IndirectSwapForm = IndirectSwapForm
  { ispCoinA    :: Text
  -- ^ One 'Coin' of the liquidity pair.
  , ispCoinB    :: Text
  -- ^ The other 'Coin'.
  , ispAmount   :: Integer
  -- ^ The amount of the first 'Coin' that should be swapped.
  , ispResult   :: Integer
  , ispSlippage :: Integer
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON)
    via PrefixedCamelCase "isp" IndirectSwapForm

data ISwapPreviewForm = ISwapPreviewForm
  { isppCoinA  :: Text
  , isppCoinB  :: Text
  , isppAmount :: Integer
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON)
    via PrefixedCamelCase "ispp" ISwapPreviewForm

data RemoveForm = RemoveForm
  { rpCoinA :: Text
  -- ^ One 'Coin' of the liquidity pair.
  , rpCoinB :: Text
  -- ^ The other 'Coin' of the liquidity pair.
  , rpFee   :: Fee
  -- ^ Numerator and denominator of the swap fee
  , rpDiff  :: Integer
  -- ^ The amount of liquidity tokens to burn in exchange for liquidity from the pool.
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON)
    via PrefixedCamelCase "rp" RemoveForm

data AddForm = AddForm
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
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON)
    via PrefixedCamelCase "ap" AddForm

