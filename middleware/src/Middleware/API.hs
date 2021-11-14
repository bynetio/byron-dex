module Middleware.API where

import           Middleware.Dex.Types       (CancelOrderParams (CancelOrderParams),
                                             CreateLiquidityOrderParams (CreateLiquidityOrderParams),
                                             CreateLiquidityPoolParams (CreateLiquidityPoolParams),
                                             CreateSellOrderParams,
                                             FundView (FundView),
                                             OrderView (..),
                                             PayoutView (PayoutView),
                                             PerformRandomParams (PerformRandomParams))
import           Middleware.PabClient.Types (ContractInstanceId)
import           Servant                    (Capture, Description, Get, JSON,
                                             Post, ReqBody, (:<|>), (:>))

type API = Capture "contract-instance-id" ContractInstanceId :> "funds"
           :> Description "List of user funds."
           :> Get '[JSON] [FundView]
      :<|> Capture "contract-instance-id" ContractInstanceId :> "collect-funds"
           :> Description "Collect user funds."
           :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "create-sell-order"
           :> Description "Create a sell order."
           :> ReqBody '[JSON] CreateSellOrderParams :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "create-liquidity-pool"
           :> Description "Create a liquidity pool"
           :> ReqBody '[JSON] CreateLiquidityPoolParams :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "create-liquidity-order"
           :> Description "Create a liquidity order."
           :> ReqBody '[JSON] CreateLiquidityOrderParams :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "orders"
           :> Description "List user orders."
           :> Get '[JSON] [OrderView]
      :<|> Capture "contract-instance-id" ContractInstanceId :> "all-orders"
           :> Description "List all orders."
           :> Get '[JSON] [OrderView]
      :<|> Capture "contract-instance-id" ContractInstanceId :> "payouts"
           :> Description "List user payouts."
           :> Get '[JSON] [PayoutView]
      :<|> Capture "contract-instance-id" ContractInstanceId :> "perform"
           :> Description "Perform match."
           :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "perform-random"
           :> Description "Perform random match."
           :> ReqBody '[JSON] PerformRandomParams  :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "stop"
           :> Description "Stop."
           :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "cancel"
           :> Description "Cancel order."
           :> ReqBody '[JSON] CancelOrderParams :> Post '[JSON] ()
