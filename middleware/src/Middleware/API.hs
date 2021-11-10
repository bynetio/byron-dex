module Middleware.API where

import           Middleware.Dex.Types       (DexOrder (..), FundView (FundView))
import           Middleware.PabClient.Types (ContractInstanceId)
import           Servant                    (Capture, Get, JSON, (:<|>), (:>))

type API = Capture "contract-instance-id" ContractInstanceId :> "funds" :> Get '[JSON] [FundView]
      :<|> Capture "contract-instance-id" ContractInstanceId :> "myOrders" :> Get '[JSON] [DexOrder]
