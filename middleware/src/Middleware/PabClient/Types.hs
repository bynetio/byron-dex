{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Middleware.PabClient.Types
  ( ContractInstanceId(..)
  , ContractState
  , Fund
  , lookupResBody
  ) where

import           Control.DeepSeq                         (NFData (rnf), rwhnf)
import           Data.Aeson
import           Data.Aeson.Types                        (parseEither, parseMaybe)
import           Data.Either.Combinators                 (mapLeft, maybeToRight, rightToMaybe)
import           Data.Text
import           Data.UUID                               (UUID)
import           Dex.WalletHistory                       (History (History), HistoryId)
import qualified Dex.WalletHistory                       as WalletHistory
import           GHC.Generics
import           Ledger                                  (AssetClass)
import           Middleware.Capability.Error
import           Plutus.PAB.Events.ContractInstanceState (observableState)
import           Plutus.PAB.Webserver.Types              (ContractInstanceClientState (cicCurrentState))
import           Servant.API


-- | Unique ID for contract instance
newtype ContractInstanceId = ContractInstanceId { unContractInstanceId :: UUID }
    deriving (Eq, Ord, Show, Generic)
    deriving newtype (FromJSONKey, ToJSONKey, ToHttpApiData, FromHttpApiData)
    deriving anyclass (FromJSON, ToJSON)

type ContractState = ContractInstanceClientState String

instance NFData ContractState where rnf = rwhnf

type Fund = (AssetClass, Integer)

type CallResult = Either Text SuccessCallResult

data SuccessCallResult = SuccessCallResult
  { contents :: Value,
    tag      :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

lookupResBody :: (FromJSON a) => HistoryId -> ContractState -> Either AppError a
lookupResBody hid state = do
  history   <- fromJSONValue . observableState . cicCurrentState $ state
  callRes   <- maybeToRight (CannotExtractHistoryId hid) (WalletHistory.lookup hid history)
  unCallRes <- EndpointCallError `mapLeft` callRes
  fromJSONValue (contents unCallRes)
  where
    fromJSONValue :: FromJSON a => Value -> Either AppError a
    fromJSONValue = mapLeft BodyParseError . parseEither parseJSON
