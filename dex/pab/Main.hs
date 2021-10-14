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
                                                      ToJSON (..), Value,
                                                      defaultOptions, fromJSON,
                                                      genericParseJSON,
                                                      genericToJSON)
import           Data.Aeson.Types                    (parseMaybe)
import           Data.Default
import qualified Data.Map                            as Map
import qualified Data.Monoid                         as Monoid
import           Data.OpenApi.Internal.Schema        (ToSchema)
import qualified Data.Semigroup                      as Semigroup
import           Data.Text
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import qualified Dex.OffChain                        as Dex
import qualified Dex.Trace                           as Trace
import           Dex.Types                           (DexContractState)
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
import           Plutus.PAB.Simulator                (Simulation,
                                                      SimulatorEffectHandlers,
                                                      logString)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Wallet.Emulator.Types               (knownWallet)
import           Wallet.Types                        (ContractInstanceId)

type ContractHistory = WH.History (Either Text Dex.DexContractState)

main :: IO ()
main = void $
  Simulator.runSimulationWith handlers $ do
    logString @(Builtin DexContracts) "Starting Uniswap PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidInit <- Simulator.activateContract (knownWallet 1) DexInit
    cs <- getState (Currency.currencySymbol . Semigroup.getLast) cidInit

    Simulator.waitUntilFinished cidInit

    logString @(Builtin DexContracts) $ "Initialization finished. Minted: " ++ show cs

    let coins = Map.fromList [(tn, AssetClass ("ff", tn)) | tn <- Trace.tokenNames]
        ada = AssetClass (adaSymbol, adaToken)

    cids <- fmap Map.fromList $
      forM Trace.wallets $ \w -> do
        cid <- Simulator.activateContract w DexContract
        logString @(Builtin DexContracts) $ "Uniswap user contract started for " ++ show w
        Simulator.waitForEndpoint cid "funds"
        Simulator.callEndpointOnInstance cid "funds" (Dex.WithHistoryId "FundsId" ())
        v <- getState contractState cid
        logString @(Builtin DexContracts) $ "initial funds in wallet " ++ show w ++ ": " ++ show v
        return (w, cid)

    liftIO getLine
    shutdown
  where
    fromJSONValue :: FromJSON a => Value -> Maybe a
    fromJSONValue = parseMaybe parseJSON

    getState :: (FromJSON a) => (a -> b) -> ContractInstanceId -> Simulation t b
    getState f = Simulator.waitForState (fmap f . fromJSONValue)

    contractState :: ContractHistory -> Maybe DexContractState
    contractState (WH.lookup "FundsId" -> Just (Right v)) = Just v
    contractState _                                       = Nothing


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
