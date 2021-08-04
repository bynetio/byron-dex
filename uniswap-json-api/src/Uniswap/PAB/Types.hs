{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Uniswap.PAB.Types
  where

import Control.DeepSeq      (NFData (rnf), rwhnf)
import Data.Aeson           (Value)
import Data.List            (find)
import Data.Text            (Text)
import Deriving.Aeson       (CustomJSON (CustomJSON), FromJSON, Generic, ToJSON)
import GHC.Generics         (Generic)
import Uniswap.Common.Utils (PrefixedCamelCase)



type Fee = (Integer, Integer)
type Instance = Text
type HistoryId = Text


data History a = History [(HistoryId, a)] [HistoryId]
  deriving (Show, Generic, FromJSON, ToJSON)

lookupHistory :: HistoryId -> History a -> Maybe a
lookupHistory hid (History hs _) = snd <$> find (\h' -> hid == fst h') (reverse hs)

data Coin = Coin
  { cCurrencySymbol :: Text
  , cTokenName      :: Text
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON)
    via PrefixedCamelCase "c" Coin

data WithHistoryId a = WithHistoryId
  { historyId :: HistoryId
  , content   :: a
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @create@-endpoint, which creates a new liquidity pool.
data CreateParams = CreateParams
  { cpCoinA   :: Coin
  -- ^ The other 'Coin'.
  , cpCoinB   :: Coin
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
  { spCoinA    :: Coin
  -- ^ One 'Coin' of the liquidity pair.
  , spCoinB    :: Coin
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
  { sppCoinA  :: Coin
  , sppCoinB  :: Coin
  , sppFee    :: Fee
  -- ^ Numerator and denominator of the swap fee
  , sppAmount :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data IndirectSwapParams = IndirectSwapParams
  { ispCoinA    :: Coin
  -- ^ One 'Coin' of the liquidity pair.
  , ispCoinB    :: Coin
    -- ^ The other 'Coin'.
  , ispAmount   :: Integer
    -- ^ The amount of the first 'Coin' that should be swapped.
  , ispResult   :: Integer
  , ispSlippage :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ISwapPreviewParams = ISwapPreviewParams
  { isppCoinA  :: Coin
  , isppCoinB  :: Coin
  , isppAmount :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @close@-endpoint, which closes a liquidity pool.
data CloseParams = CloseParams
  { clpCoinA :: Coin
  -- ^ One 'Coin' of the liquidity pair.
  , clpCoinB :: Coin
  -- ^ The other 'Coin' of the liquidity pair.
  , clpFee   :: Fee
  -- ^ Numerator and denominator of the swap fee
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @remove@-endpoint, which removes some liquidity from a liquidity pool.
data RemoveParams = RemoveParams
  { rpCoinA :: Coin
  -- ^ One 'Coin' of the liquidity pair.
  , rpCoinB :: Coin
  -- ^ The other 'Coin' of the liquidity pair.
  , rpFee   :: Fee
  -- ^ Numerator and denominator of the swap fee
  , rpDiff  :: Integer
  -- ^ The amount of liquidity tokens to burn in exchange for liquidity from the pool.
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @add@-endpoint, which adds liquidity to a liquidity pool in exchange for liquidity tokens.
data AddParams = AddParams
  { apCoinA   :: Coin
  -- ^ One 'Coin' of the liquidity pair.
  , apCoinB   :: Coin
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
  { clsRemoveId :: HistoryId
    -- ^ Identifier of History that should be removed from state
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data UniswapStatusResponse = UniswapStatusResponse
  { cicCurrentState :: UniswapCurrentState,
    cicContract     :: UniswapContract,
    cicWallet       :: UniswapWallet,
    cicDefintion    :: UniswapDefinition
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance NFData UniswapStatusResponse where rnf = rwhnf
data UniswapHook = UniswapHook
  { rqID      :: Integer,
    itID      :: Integer,
    rqRequest :: Value
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data UniswapCurrentState = UniswapCurrentState
  { observableState :: History UniswapMethodResult,
    hooks           :: [UniswapHook],
    err             :: Maybe Text,
    logs            :: [UniswapLog],
    lastLogs        :: [UniswapLog]
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data UniswapLog = UniswapLog
  { _logMessageContent :: Text,
    _logLevel          :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)


data UniswapDefinition = UniswapDefiniotion
  { contents :: Value,
    tag      :: Text

  } deriving (Show, Generic, FromJSON, ToJSON)

-- | add ADT to handle all possible types in Result
type UniswapMethodResult = Either Text UniswapSuccessMethodResult


data UniswapSuccessMethodResult = UniswapSuccessMethodResult
  { contents :: Maybe Value,
    tag      :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

newtype UniswapContract = UniswapContract
  { unContractInstanceId :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

newtype UniswapWallet = UniswapWallet
  { getWallet :: Integer
  }
  deriving (Show, Generic, FromJSON, ToJSON)
