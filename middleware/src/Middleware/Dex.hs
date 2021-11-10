
module Middleware.Dex where

import           Colog.Polysemy.Formatting.WithLog (WithLog)
import           Ledger                            (AssetClass)
import           Ledger.Value                      (assetClass, unAssetClass)
import           Middleware.API                    (API)
import           Middleware.Capability.Error
import           Middleware.Dex.Types              hiding (Error)
import           Middleware.PabClient              (ManagePabClient,
                                                    createLiquidityPoolInPab,
                                                    getFunds)
import           Middleware.PabClient.Types        hiding (Error)
import           Polysemy
import           Servant
import           Servant.Polysemy.Server


data Dex r a where
  Funds :: ContractInstanceId -> Dex r [FundView]
  CreateLiquidityPool :: ContractInstanceId -> CreateLiquidityPoolParams -> Dex r ()

makeSem ''Dex

runDex :: (WithLog r, Members '[ManagePabClient] r)
       => Sem (Dex ': r) a
       -> Sem r a
runDex = interpret
  (\case
      Funds cid -> do
        fs <- getFunds cid
        pure $ fmap (\(assetClass, amount') ->
                       let coin' = uncurry Coin $ unAssetClass assetClass
                           fund' = FundView coin' amount'
                       in fund'
                       ) fs
      CreateLiquidityPool cid params -> do
        createLiquidityPoolInPab cid params
  )


dexServer :: Members '[Error AppError, Dex] r => ServerT API (Sem r)
dexServer = funds
       :<|> createLiquidityPool
