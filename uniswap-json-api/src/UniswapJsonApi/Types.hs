{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UniswapJsonApi.Types where

import Control.Monad.Except     (ExceptT, MonadError)
import Control.Monad.Reader     (MonadIO, MonadReader, ReaderT)
import Data.Aeson
import Data.ByteString          (ByteString)
import Data.List                (find)
import Data.Text                (Text)
import Data.UUID                (UUID)
import GHC.Generics
import Network.Wai.Handler.Warp (HostPreference)
import Servant                  (ServerError)

type Instance = Text

type OperationId = UUID

data History a = History [(OperationId, a)] [OperationId]
  deriving (Show, Generic, FromJSON, ToJSON)

lookupHistory :: OperationId -> History a -> Maybe a
lookupHistory opId (History hs _) = snd <$> find (\h' -> opId == fst h') (reverse hs)

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
  { observableState :: History (Either Text UniswapDefinition),
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



data PabConfig = MkPabConfig
  { pabUrl  :: String
  , pabPort :: Int
  } deriving Show

data AppContext = MkAppContext
  { pab   :: PabConfig
  , port  :: Int
  } deriving (Show)

newtype (MonadIO m) => AppM m a = AppM
  { unAppM :: ReaderT AppContext (ExceptT ServerError m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader AppContext
             , MonadError ServerError
             , MonadIO
             )

