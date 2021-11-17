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
import           Servant                    (Capture, Get, JSON, Post, ReqBody,
                                             (:<|>), (:>))

type API = Capture "contract-instance-id" ContractInstanceId :> "funds"                                                                :> Get  '[JSON] [FundView]
      :<|> Capture "contract-instance-id" ContractInstanceId :> "collect-funds"                                                        :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "create-sell-order"      :> ReqBody '[JSON] CreateSellOrderParams      :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "create-liquidity-pool"  :> ReqBody '[JSON] CreateLiquidityPoolParams  :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "create-liquidity-order" :> ReqBody '[JSON] CreateLiquidityOrderParams :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "orders"                                                               :> Get  '[JSON] [OrderView]
      :<|> Capture "contract-instance-id" ContractInstanceId :> "all-orders"                                                           :> Get  '[JSON] [OrderView]
      :<|> Capture "contract-instance-id" ContractInstanceId :> "payouts"                                                              :> Get  '[JSON] [PayoutView]
      :<|> Capture "contract-instance-id" ContractInstanceId :> "perform"                                                              :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "perform-random"         :> ReqBody '[JSON] PerformRandomParams        :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "stop"                                                                 :> Post '[JSON] ()
      :<|> Capture "contract-instance-id" ContractInstanceId :> "cancel"                 :> ReqBody '[JSON] CancelOrderParams          :> Post '[JSON] ()
