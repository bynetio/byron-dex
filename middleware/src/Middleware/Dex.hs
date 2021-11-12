
module Middleware.Dex where

import           Colog.Polysemy.Formatting.WithLog (WithLog)
import           Ledger                            (AssetClass)
import           Ledger.Value                      (assetClass, unAssetClass)
import           Middleware.API                    (API)
import           Middleware.Capability.Error
import           Middleware.Dex.Types              hiding (Error)
import           Middleware.PabClient              (ManagePabClient,
                                                    cancelOrder, createSellOrder,
                                                    createLiquidityPoolInPab,
                                                    getFunds, getMyOrders)
import           Middleware.PabClient.Types        hiding (Error)
import           Polysemy
import           Servant
import           Servant.Polysemy.Server
import           Servant.Server                    (ServerT)

data Dex r a where
  Funds               :: ContractInstanceId -> Dex r [FundView]
  CreateSellOrderEU   :: ContractInstanceId -> CreateSellOrderParams -> Dex r ()
  CreateLiquidityPool :: ContractInstanceId -> CreateLiquidityPoolParams -> Dex r ()
  MyOrders            :: ContractInstanceId -> Dex r [DexOrder]
  Cancel              :: ContractInstanceId -> MidCancelOrder -> Dex r ()

makeSem ''Dex

runDex :: (WithLog r, Members '[ManagePabClient] r)
       => Sem (Dex ': r) a
       -> Sem r a
runDex = interpret
  (\case
      Funds cid -> do
        fs <- getFunds cid
        pure $ fmap (uncurry fundView) fs
      CreateSellOrderEU cid params -> do
        createSellOrder cid params
      CreateLiquidityPool cid params -> do
        createLiquidityPoolInPab cid params
      MyOrders cid -> do
        os <- getMyOrders cid
        pure $ fmap dexOrder os
      Cancel cid params -> do
        cancelOrder cid params
  )

dexServer :: Members '[Error AppError, Dex] r => ServerT API (Sem r)
dexServer = funds
  :<|> createSellOrderEU
  :<|> createLiquidityPool  
  :<|> myOrders
  :<|> cancel
