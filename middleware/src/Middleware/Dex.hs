
module Middleware.Dex where

import Colog.Polysemy.Formatting.WithLog (WithLog)
import Dex.Types                         (PayoutSummary (PayoutSummary))
import Ledger                            (AssetClass)
import Ledger.Value                      (assetClass, unAssetClass)
import Middleware.API                    (API)
import Middleware.Capability.Error
import Middleware.Dex.Types              hiding (Error)
import Middleware.PabClient              (ManagePabClient, cancelOrder, collectFunds,
                                          createLiquidityPoolInPab, createSellOrder, getFunds, getMyOrders,
                                          getMyPayouts, stop)
import Middleware.PabClient.Types        hiding (Error)
import Polysemy
import Servant
import Servant.Polysemy.Server
import Servant.Server                    (ServerT)

data Dex r a where
  Funds               :: ContractInstanceId -> Dex r [FundView]
  CreateSellOrderEU   :: ContractInstanceId -> CreateSellOrderParams -> Dex r ()
  CreateLiquidityPool :: ContractInstanceId -> CreateLiquidityPoolParams -> Dex r ()
  MyOrders            :: ContractInstanceId -> Dex r [DexOrder]
  Cancel              :: ContractInstanceId -> MidCancelOrder -> Dex r ()
  Collect             :: ContractInstanceId -> Dex r ()
  StopContract        :: ContractInstanceId -> Dex r ()
  Payouts             :: ContractInstanceId -> Dex r [PayoutView]

makeSem ''Dex

runDex :: (WithLog r, Members '[ManagePabClient] r)
       => Sem (Dex ': r) a
       -> Sem r a
runDex = interpret
  (\case
      Funds cid -> do
        fs <- getFunds cid
        pure $ fmap (uncurry mkFundView) fs
      CreateSellOrderEU cid params ->
        createSellOrder cid params
      MyOrders cid -> do
        os <- getMyOrders cid
        pure $ fmap dexOrder os
      CreateLiquidityPool cid params ->
        createLiquidityPoolInPab cid params
      Cancel cid params ->
        cancelOrder cid params
      Collect cid ->
        collectFunds cid
      StopContract cid ->
        stop cid
      Payouts cid -> do
        (PayoutSummary ps) <- getMyPayouts cid
        pure $ fmap (uncurry mkPayoutView) ps
  )

dexServer :: Members '[Error AppError, Dex] r => ServerT API (Sem r)
dexServer = funds
       :<|> createSellOrderEU
       :<|> createLiquidityPool
       :<|> myOrders
       :<|> cancel
       :<|> collect
       :<|> stopContract
       :<|> payouts
