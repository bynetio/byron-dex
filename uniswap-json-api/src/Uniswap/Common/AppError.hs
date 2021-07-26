{-# LANGUAGE OverloadedStrings #-}
module Uniswap.Common.AppError
  where

import qualified Control.Monad.Except      as MTL
import           Control.Monad.Freer       (type (~>))
import           Control.Monad.Freer.Error (Error (..))
import           Data.Text                 (Text)
import           Servant.Client.Streaming  (ClientError)
import           UniswapJsonApi.Types

type AppError = Error Err

data Err
  = StatusNotFound Instance OperationId
  | CallEndpointFailed Instance OperationId ClientError
  | CallStatusFailed Instance ClientError
  | GetStatusFailed ClientError
  | GetFundsFailed ClientError
  | GetFundsFailedRes Text
  | EndpointRequestFailed Text ClientError
  | EndpointRequestFailedRes Text Text
  deriving (Eq, Show)

