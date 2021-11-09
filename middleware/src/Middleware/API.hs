module Middleware.API where

import Middleware.Dex.Types       (FundView (FundView))
import Middleware.PabClient.Types (ContractInstanceId)
import Servant                    (Capture, Get, JSON, type (:>))

type API = Capture "contract-instance-id" ContractInstanceId :> "funds" :> Get '[JSON] [FundView]
