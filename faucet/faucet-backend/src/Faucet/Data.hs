{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Faucet.Data (AddressParam(..), TokenName(..), Token(..), TokenCurrency(..), FaucetException(..)) where

import           Control.Exception (Exception)
import           Data.Aeson        (FromJSON, ToJSON)
import           Data.Typeable     (Typeable)
import           GHC.Generics      (Generic)
import           Servant           (FromHttpApiData)

newtype AddressParam = AddressParam { unAddress :: String }
  deriving (Eq, Show, Generic)
  deriving newtype (FromHttpApiData)

newtype TokenName = TokenName { unTokenName :: String }
  deriving (Eq, Show, Generic)
  deriving newtype (FromHttpApiData, ToJSON, FromJSON, Ord)

newtype TokenCurrency = TokenCurrency { unTokenCurrency :: String }
  deriving (Eq, Show, Generic)
  deriving newtype (ToJSON)

data Token = Token { tokenCurrency :: TokenCurrency, tokenName :: TokenName }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data FaucetException
  = SubmitTxError String
  | SomeError String
  | QueryProtocolParamsError String
  | QueryUtxoError String
  | UknownNetworkIdError String
  | AddressDecodingError String
  | SkeyDeserialiseError String
  | TokenNameNotSupportedError TokenName
  | NoUtxoToConsumeError
  deriving (Show, Typeable)

instance Exception FaucetException
