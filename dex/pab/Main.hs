{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}
module Main
  ( main
  ) where

import           Control.Monad                       (forM, void)
import           Control.Monad.Freer                 (Eff, Member, interpret,
                                                      type (~>))
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON (..),
                                                      Options (..), Result (..),
                                                      ToJSON (..),
                                                      defaultOptions, fromJSON,
                                                      genericParseJSON,
                                                      genericToJSON)
import           Data.Default
import qualified Data.Map                            as Map
import qualified Data.Monoid                         as Monoid
import           Data.OpenApi.Internal.Schema        (ToSchema)
import qualified Data.Semigroup                      as Semigroup
import           Data.Text
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import qualified Dex.OffChain                        as Dex
import qualified Dex.Trace                           as Trace
import qualified Dex.Types                           as Dex
import qualified Dex.WalletHistory                   as WH
import           GHC.Generics                        (Generic)
import           Ledger.Ada                          (adaSymbol, adaToken)
import           Ledger.Value                        (AssetClass (..))
import           Plutus.Contract                     (ContractError, Empty,
                                                      awaitPromise)
import qualified Plutus.Contracts.Currency           as Currency
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..),
                                                      type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers,
                                                      logString)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Wallet.Emulator.Types               (knownWallet)

main :: IO ()
main = void $
  Simulator.runSimulationWith handlers $ do
    logString @(Builtin DexContracts) "Starting Uniswap PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidInit <- Simulator.activateContract (knownWallet 1) DexInit
    cs <- flip Simulator.waitForState cidInit $ \json -> case fromJSON json of
      Success (Just (Semigroup.Last cur)) -> Just $ Currency.currencySymbol cur
      _                                   -> Nothing
    _ <- Simulator.waitUntilFinished cidInit

    logString @(Builtin DexContracts) $ "Initialization finished. Minted: " ++ show cs


    let coins = Map.fromList [(tn, AssetClass ("ff", tn)) | tn <- Trace.tokenNames]
        ada = AssetClass (adaSymbol, adaToken)

    cids <- fmap Map.fromList $
      forM Trace.wallets $ \w -> do
        cid <- Simulator.activateContract w DexContract
        logString @(Builtin DexContracts) $ "Uniswap user contract started for " ++ show w
        Simulator.waitForEndpoint cid "funds"
        _ <- Simulator.callEndpointOnInstance cid "funds" (Dex.Request "FundsId" 0 ())
        v <- flip Simulator.waitForState cid $ \json -> case (fromJSON json :: Result (WH.History (Either Text Dex.DexContractState))) of
          Success (WH.lookup "FundsId" -> Just (Right v)) -> Just v
          _                                               -> Nothing
        logString @(Builtin DexContracts) $ "initial funds in wallet " ++ show w ++ ": " ++ show v
        return (w, cid)

    _ <- liftIO getLine
    shutdown

data DexContracts = DexContract | DexInit deriving (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance Pretty DexContracts where
  pretty = viaShow

instance Builtin.HasDefinitions DexContracts where
    getDefinitions = [DexContract]
    getSchema = \case
      DexContract -> Builtin.endpointsToSchemas @Dex.DexSchema
      DexInit     -> Builtin.endpointsToSchemas @Empty
    getContract = \case
      DexContract -> SomeBuiltin (awaitPromise Dex.dexEndpoints)
      DexInit     -> SomeBuiltin Trace.setupTokens

handlers :: SimulatorEffectHandlers (Builtin DexContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (Builtin.contractHandler (Builtin.handleBuiltin @DexContracts))
