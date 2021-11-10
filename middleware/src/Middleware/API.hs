module Middleware.API where

import Middleware.Dex.Types       (CreateLiquidityPoolParams (CreateLiquidityPoolParams), DexOrder (..),
                                   FundView (FundView))
import Middleware.PabClient.Types (ContractInstanceId)
import Servant                    (Capture, Get, JSON, Post, ReqBody, (:<|>), (:>))

type API = Capture "contract-instance-id" ContractInstanceId :> "funds" :> Get '[JSON] [FundView]
      :<|> Capture "contract-instance-id" ContractInstanceId :> "create-liquidity-pool" :> ReqBody '[JSON] CreateLiquidityPoolParams :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "my-orders" :> Get '[JSON] [DexOrder]
