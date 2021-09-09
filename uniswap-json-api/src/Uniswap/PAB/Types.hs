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
  { currencySymbol :: Text,
    tokenName      :: Text
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON)

data WithHistoryId a = WithHistoryId
  { historyId :: HistoryId,
    content   :: a
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @create@-endpoint, which creates a new liquidity pool.
data CreateParams = CreateParams
  { -- | The other 'Coin'.
    coinA   :: Coin,
    -- | One 'Coin' of the liquidity pair.
    coinB   :: Coin,
    -- | Numerator and denominator of the swap fee
    fee     :: Fee,
    -- | Amount of liquidity for the first 'Coin'.
    amountA :: Integer,
    -- | Amount of liquidity for the second 'Coin'.
    amountB :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @swap@-endpoint, which allows swaps between the two different coins in a liquidity pool.
-- One of the provided amounts must be positive, the other must be zero.
data SwapParams = SwapParams
  { -- | One 'Coin' of the liquidity pair.
    coinA    :: Coin,
    -- | The other 'Coin'.
    coinB    :: Coin,
    -- | Numerator and denominator of the swap fee
    fee      :: Fee,
    -- | The amount the first 'Coin' that should be swapped.
    amount   :: Integer,
    -- | The expected amount of swaped 'Text' (quoted amount)
    result   :: Integer,
    -- | The expected % difference between quoted and executed prices.
    slippage :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data SwapPreviewParams = SwapPreviewParams
  { coinA  :: Coin,
    coinB  :: Coin,
    -- | Numerator and denominator of the swap fee
    fee    :: Fee,
    amount :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data IndirectSwapParams = IndirectSwapParams
  { -- | One 'Coin' of the liquidity pair.
    coinA    :: Coin,
    -- | The other 'Coin'.
    coinB    :: Coin,
    -- | The amount of the first 'Coin' that should be swapped.
    amount   :: Integer,
    result   :: Integer,
    slippage :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ISwapPreviewParams = ISwapPreviewParams
  { coinA  :: Coin,
    coinB  :: Coin,
    amount :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @close@-endpoint, which closes a liquidity pool.
data CloseParams = CloseParams
  { -- | One 'Coin' of the liquidity pair.
    coinA :: Coin,
    -- | The other 'Coin' of the liquidity pair.
    coinB :: Coin,
    -- | Numerator and denominator of the swap fee
    fee   :: Fee
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @remove@-endpoint, which removes some liquidity from a liquidity pool.
data RemoveParams = RemoveParams
  { -- | One 'Coin' of the liquidity pair.
    coinA :: Coin,
    -- | The other 'Coin' of the liquidity pair.
    coinB :: Coin,
    -- | Numerator and denominator of the swap fee
    fee   :: Fee,
    -- | The amount of liquidity tokens to burn in exchange for liquidity from the pool.
    diff  :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @add@-endpoint, which adds liquidity to a liquidity pool in exchange for liquidity tokens.
data AddParams = AddParams
  { -- | One 'Coin' of the liquidity pair.
    coinA   :: Coin,
    -- | The other 'Coin' of the liquidity pair.
    coinB   :: Coin,
    -- | Numerator and denominator of the swap fee
    fee     :: Fee,
    -- | The amount of coins of the first kind to add to the pool.
    amountA :: Integer,
    -- | The amount of coins of the second kind to add to the pool.
    amountB :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @clearState-@endpoint, which removes entry from the state corresponding to given OperationId
newtype ClearStateParams = ClearStateParams
  { -- | Identifier of History that should be removed from state
    removeId :: HistoryId
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
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data UniswapDefinition = UniswapDefiniotion
  { contents :: Value,
    tag      :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

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
