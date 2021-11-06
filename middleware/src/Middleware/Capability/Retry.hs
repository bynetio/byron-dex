{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Middleware.Capability.Retry where

import qualified Control.Concurrent           as Concurrent
import           Control.Monad.IO.Class       (MonadIO)
import           Data.Aeson                   (FromJSON)
import           Data.Aeson.Types             ((.:))
import qualified Data.Aeson.Types             as Aeson
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Middleware.Capability.Logger (Logger, logDebug, logError)
import           Middleware.Capability.Time   (Time, sleep)
import           Polysemy
import           Servant.API
import           Servant.Client



data PABError = PABError
    { error   :: Text
    , message :: Text
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data PABRequestError
    = HttpError BaseUrl ClientError
    | ApiError BaseUrl PABError
  deriving stock (Show)

newtype RawPabResBody a = RawPabResBody
    { unRawPabResBody :: Either PABRequestError a
    }
  deriving stock (Show, Eq)

instance FromJSON a => FromJSON (RawPabResBody a) where
    parseJSON = Aeson.withObject "RawPabResBody" $ \o -> do -- extract logic
        success <- o .: "success"
        RawPabResBody . Right <$> o .: "data"
            -- if success
            --     then Right <$> o .: "data"
            --     else Left <$> Aeson.parseJSON (Aeson.Object o)

type RawPabResponse a = ResponseF (RawPabResBody a)

data PabResponse a =
  PabResponse
    { rawResponse    :: RawPabResponse a
    , bodyOfResponse :: a
    }

data PabClientAPI a where
    Status :: FromJSON a => PabClientAPI (PabResponse a)
    Endpoint :: FromJSON a => PabClientAPI (PabResponse a)
    Stop :: PabClientAPI (PabResponse ())

runPabClient
    :: Members '[Logger, Error PABRequestError] r
    => Sem (PabClientAPI ': r) a
    -> Sem r a
runPabClient = undefined
