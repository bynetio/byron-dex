module Middleware.API where

import           Middleware.Dex.Types       (CreateLiquidityPoolParams (CreateLiquidityPoolParams),
                                             DexOrder (..), FundView (FundView))
import           Middleware.PabClient.Types (ContractInstanceId)
import           Servant                    (Capture, Get, JSON, (:<|>), (:>))

type API = Capture "contract-instance-id" ContractInstanceId :> "funds" :> Get '[JSON] [FundView]
      :<|> Capture "contract-instance-id" ContractInstanceId :> "createLiquidityPool" :> ReqBody '[JSON] CreateLiquidityPoolParams :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "myOrders" :> Get '[JSON] [DexOrder]
