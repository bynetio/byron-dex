module Middleware.API where

import Middleware.Dex.Types       (CreateLiquidityPoolParams (CreateLiquidityPoolParams),
                                   CreateSellOrderParams, DexOrder (..), FundView (FundView),
                                   MidCancelOrder (MidCancelOrder), PayoutView (PayoutView))
import Middleware.PabClient.Types (ContractInstanceId)
import Servant                    (Capture, Get, JSON, Post, ReqBody, (:<|>), (:>))

type API = Capture "contract-instance-id" ContractInstanceId :> "funds" :> Get '[JSON] [FundView]
      :<|> Capture "contract-instance-id" ContractInstanceId :> "create-sell-order" :> ReqBody '[JSON] CreateSellOrderParams :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "create-liquidity-pool" :> ReqBody '[JSON] CreateLiquidityPoolParams :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "orders" :> Get '[JSON] [DexOrder]
      :<|> Capture "contract-instance-id" ContractInstanceId :> "cancel" :> ReqBody '[JSON] MidCancelOrder :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "collect-funds" :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "stop" :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "payouts" :> Get '[JSON] [PayoutView]
