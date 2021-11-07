{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |

module Middleware.API where

import Data.Aeson
import GHC.Generics
import Ledger                      (AssetClass)
import Middleware.Capability.Error
import Middleware.PabClient
import Middleware.PabClient.Types
import Polysemy
import Servant

-- | Move 'FundView' Data Type to separate module
data FundView = FundView
  { coin :: AssetClass
  , amount :: Integer
  } deriving (Show, Generic, ToJSON)


type API = Capture "contract-instance-id" ContractInstanceId :> "funds" :> Get '[JSON] [FundView]

-- | Move ''
data Dex r a where
  Funds :: ContractInstanceId -> Dex r [FundView]

makeSem ''Dex

runDex ::(Members '[PabClient] r)
       => Sem (Dex ': r) a
       -> Sem r a
runDex = interpret
  (\case
      Funds cid -> pure []
  )

-- | Move 'server' to separate module
dexServer :: Members '[Error AppError, Dex] r => ServerT API (Sem r)
dexServer = funds
