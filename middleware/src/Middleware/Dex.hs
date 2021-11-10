
module Middleware.Dex where

import           Colog.Polysemy.Formatting.WithLog (WithLog)
import           Ledger.Value                      (unAssetClass)
import           Middleware.API                    (API)
import           Middleware.Capability.Error
import           Middleware.Dex.Types              hiding (Error)
import           Middleware.PabClient              (ManagePabClient, getFunds,
                                                    getMyOrders)
import           Middleware.PabClient.Types        hiding (Error)
import           Polysemy
import           Servant                           ((:<|>) ((:<|>)))
import           Servant.Server                    (ServerT)


data Dex r a where
  Funds    :: ContractInstanceId -> Dex r [FundView]
  MyOrders :: ContractInstanceId -> Dex r [DexOrder]

makeSem ''Dex

runDex :: (WithLog r, Members '[ManagePabClient] r)
       => Sem (Dex ': r) a
       -> Sem r a
runDex = interpret $
  \case
      Funds cid -> do
        fs <- getFunds cid
        pure $ fmap (uncurry fundView) fs
      MyOrders cid -> do
        os <- getMyOrders cid
        pure $ fmap dexOrder os

dexServer :: Members '[Error AppError, Dex] r => ServerT API (Sem r)
dexServer = funds :<|> myOrders
