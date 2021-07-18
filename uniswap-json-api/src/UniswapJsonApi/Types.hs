{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE FlexibleContexts           #-}
module UniswapJsonApi.Types where

import Control.Monad.Except     (ExceptT, MonadError)
import Control.Monad.Reader     (MonadIO, MonadReader, ReaderT)
import Data.ByteString          (ByteString)
import Data.Text                (Text)
import Network.Wai.Handler.Warp (HostPreference)
import Servant

type Instance = Text

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

