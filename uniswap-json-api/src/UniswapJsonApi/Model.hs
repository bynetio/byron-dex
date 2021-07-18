{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module UniswapJsonApi.Model where

import           Data.Aeson
import           Data.Text
import           GHC.Generics

data Config = Config
  { _port    :: Int,
    _apiUrl  :: String,
    _apiPort :: Int
  }

type PABResponse a = Either Text a

data UniswapStatusResponse = UniswapStatusResponse
  { cicCurrentState :: UniswapCurrentState,
    cicContract     :: UniswapContract,
    cicWallet       :: UniswapWallet,
    cicDefintion    :: UniswapDefinition
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data UniswapHook = UniswapHook
  { rqID      :: Integer,
    itID      :: Integer,
    rqRequest :: Value
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data UniswapCurrentState = UniswapCurrentState
  { observableState :: ([(Text,Either Text UniswapDefinition)],[Text]),
    hooks           :: [UniswapHook],
    err             :: Maybe Text,
    logs            :: [Text],
    lastLogs        :: [Text]
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

data UniswapDefinition = UniswapDefinition
  { contents :: Value,
    tag      :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)
