{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |

module Middleware.PabClient.Types
  ( ContractInstanceId(..)
  , ContractState
  , Fund
  ) where

import Control.DeepSeq            (NFData (rnf), rwhnf)
import Data.Aeson
import Data.UUID                  (UUID)
import GHC.Generics
import Ledger                     (AssetClass)
import Plutus.PAB.Webserver.Types (ContractInstanceClientState)
import Servant.API



-- | Unique ID for contract instance
newtype ContractInstanceId = ContractInstanceId { unContractInstanceId :: UUID }
    deriving (Eq, Ord, Show, Generic)
    deriving newtype (FromJSONKey, ToJSONKey, ToHttpApiData)
    deriving anyclass (FromJSON, ToJSON)

type ContractState = ContractInstanceClientState String

instance NFData ContractState where rnf = rwhnf

type Fund = (AssetClass, Integer)
