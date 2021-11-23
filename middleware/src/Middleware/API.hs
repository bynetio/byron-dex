module Middleware.API where

import           Middleware.Dex.Types       (CancelOrderParams (CancelOrderParams),
                                             CoinSet (CoinSet),
                                             CreateLiquidityOrderParams (CreateLiquidityOrderParams),
                                             CreateLiquidityPoolParams (CreateLiquidityPoolParams),
                                             CreateSellOrderParams,
                                             FundView (FundView),
                                             OrderView (..),
                                             PayoutView (PayoutView),
                                             PerformRandomParams (PerformRandomParams))
import           Middleware.PabClient.Types (ContractInstanceId)
import           Servant                    (Capture, Description, Get, JSON,
                                             NoContent, Post, PostAccepted,
                                             PostCreated, ReqBody, (:<|>), (:>))
import           Servant.Swagger.UI         (SwaggerSchemaUI)

type API =
  Capture "contract-instance-id" ContractInstanceId :> "funds"
    :> Description "List of user funds."
    :> Get '[JSON] [FundView]
    :<|> Capture "contract-instance-id" ContractInstanceId :> "collect-funds"
      :> Description "Collect user funds."
      :> PostAccepted '[JSON] ()
    :<|> Capture "contract-instance-id" ContractInstanceId :> "create-sell-order"
      :> Description "Create a sell order."
      :> ReqBody '[JSON] CreateSellOrderParams
      :> PostCreated '[JSON] ()
    :<|> Capture "contract-instance-id" ContractInstanceId :> "create-liquidity-pool"
      :> Description "Create a liquidity pool"
      :> ReqBody '[JSON] CreateLiquidityPoolParams
      :> PostCreated '[JSON] ()
    :<|> Capture "contract-instance-id" ContractInstanceId :> "create-liquidity-order"
      :> Description "Create a liquidity order."
      :> ReqBody '[JSON] CreateLiquidityOrderParams
      :> PostCreated '[JSON] ()
    :<|> Capture "contract-instance-id" ContractInstanceId :> "orders"
      :> Description "List user orders."
      :> Get '[JSON] [OrderView]
    :<|> Capture "contract-instance-id" ContractInstanceId :> "all-orders"
      :> Description "List all orders."
      :> Get '[JSON] [OrderView]
    :<|> Capture "contract-instance-id" ContractInstanceId :> "orders-by-set"
      :> Description "List orders by coin sets"
      :> ReqBody '[JSON] CoinSet
      :> Post '[JSON] [OrderView]
    :<|> Capture "contract-instance-id" ContractInstanceId :> "sets"
      :> Description "Get all unique sets of coins"
      :> Get '[JSON] [CoinSet]
    :<|> Capture "contract-instance-id" ContractInstanceId :> "payouts"
      :> Description "List user payouts."
      :> Get '[JSON] [PayoutView]
    :<|> Capture "contract-instance-id" ContractInstanceId :> "perform"
      :> Description "Perform match."
      :> PostAccepted '[JSON] ()
    :<|> Capture "contract-instance-id" ContractInstanceId :> "perform-random"
      :> Description "Perform random match."
      :> ReqBody '[JSON] PerformRandomParams
      :> PostAccepted '[JSON] ()
    :<|> Capture "contract-instance-id" ContractInstanceId :> "stop"
      :> Description "Stop."
      :> PostAccepted '[JSON] ()
    :<|> Capture "contract-instance-id" ContractInstanceId :> "cancel"
      :> Description "Cancel order."
      :> ReqBody '[JSON] CancelOrderParams
      :> PostAccepted '[JSON] ()

type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

type DexAPI = API :<|> SwaggerAPI
