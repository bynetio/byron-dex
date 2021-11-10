module Middleware.API where

import           Servant

import           Middleware.Dex.Types       (CreateLiquidityPoolParams (CreateLiquidityPoolParams),
                                             FundView (FundView))
import           Middleware.PabClient.Types (ContractInstanceId)

type API = Capture "contract-instance-id" ContractInstanceId :> "funds" :> Get '[JSON] [FundView]
      :<|> Capture "contract-instance-id" ContractInstanceId :> "createLiquidityPool" :> ReqBody '[JSON] CreateLiquidityPoolParams :> Post '[JSON] ()
