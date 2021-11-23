
module Middleware.Dex where

import           Colog.Polysemy.Formatting.WithLog (WithLog)
import           Dex.Types                         (PayoutSummary (PayoutSummary))
import           Ledger                            (AssetClass)
import           Ledger.Value                      (assetClass, unAssetClass)
import           Middleware.API                    (API)
import           Middleware.Capability.Error
import           Middleware.Dex.Types              hiding (Error)
import           Middleware.PabClient              (ManagePabClient,
                                                    cancelOrder, collectFunds,
                                                    createLiquidityOrderInPab,
                                                    createLiquidityPoolInPab,
                                                    createSellOrder,
                                                    getAllOrders, getFunds,
                                                    getMyOrders, getMyPayouts,
                                                    getOrdersBySet, getSets,
                                                    performInPab,
                                                    performNRandomInPab, stop)
import           Middleware.PabClient.Types        hiding (Error)
import           Polysemy
import           Servant
import           Servant.Polysemy.Server
import           Servant.Server                    (ServerT)

data Dex r a where
  Funds                :: ContractInstanceId -> Dex r [FundView]
  Collect              :: ContractInstanceId -> Dex r ()
  CreateSellOrderEU    :: ContractInstanceId -> CreateSellOrderParams -> Dex r ()
  CreateLiquidityPool  :: ContractInstanceId -> CreateLiquidityPoolParams -> Dex r ()
  CreateLiquidityOrder :: ContractInstanceId -> CreateLiquidityOrderParams -> Dex r ()
  MyOrders             :: ContractInstanceId -> Dex r [OrderView]
  AllOrders            :: ContractInstanceId -> Dex r [OrderView]
  OrdersBySet          :: ContractInstanceId -> CoinSet -> Dex r [OrderView]
  Sets                 :: ContractInstanceId -> Dex r [CoinSet]
  Payouts              :: ContractInstanceId -> Dex r [PayoutView]
  Perform              :: ContractInstanceId -> Dex r ()
  PerformNRandom       :: ContractInstanceId -> PerformRandomParams -> Dex r ()
  StopContract         :: ContractInstanceId -> Dex r ()
  Cancel               :: ContractInstanceId -> CancelOrderParams -> Dex r ()

makeSem ''Dex

runDex :: (WithLog r, Members '[ManagePabClient] r)
       => Sem (Dex ': r) a
       -> Sem r a
runDex = interpret
  (\case
      Funds cid -> do
        fs <- getFunds cid
        pure $ fmap (uncurry mkFundView) fs
      Collect cid ->
        collectFunds cid
      CreateSellOrderEU cid params ->
        createSellOrder cid params
      CreateLiquidityPool cid params ->
        createLiquidityPoolInPab cid params
      CreateLiquidityOrder cid params -> do
        createLiquidityOrderInPab cid params
      MyOrders cid -> do
        os <- getMyOrders cid
        pure $ fmap dexOrder os
      AllOrders cid -> do
        os <- getAllOrders cid
        pure $ fmap dexOrder os
      OrdersBySet cid params -> do
        os <- getOrdersBySet cid params
        pure $ fmap dexOrder os
      Sets cid -> do
        ss <- getSets cid
        pure $ fmap mkCoinSet ss
      Payouts cid -> do
        (PayoutSummary ps) <- getMyPayouts cid
        pure $ fmap (uncurry mkPayoutView) ps
      Perform cid -> do
        performInPab cid
      PerformNRandom cid n -> do
        performNRandomInPab cid n
      StopContract cid ->
        stop cid
      Cancel cid params ->
        cancelOrder cid params
  )

dexServer :: Members '[Error AppError, Dex] r => ServerT API (Sem r)
dexServer = funds
       :<|> collect
       :<|> createSellOrderEU
       :<|> createLiquidityPool
       :<|> createLiquidityOrder
       :<|> myOrders
       :<|> allOrders
       :<|> ordersBySet
       :<|> sets
       :<|> payouts
       :<|> perform
       :<|> performNRandom
       :<|> stopContract
       :<|> cancel
