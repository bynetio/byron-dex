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
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}
module Main (main) where

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
import qualified Data.Map                            as Map
import qualified Data.Monoid                         as Monoid
import qualified Data.Semigroup                      as Semigroup
import           Data.Text
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import           Ledger.Ada                          (adaSymbol, adaToken)
import           Plutus.Contract                     (ContractError, Empty)
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
import qualified Uniswap.Common.WalletHistory        as WH
import qualified Uniswap.OffChain                    as Uniswap
import qualified Uniswap.Trace                       as Uniswap
import qualified Uniswap.Types                       as Uniswap
import           Wallet.Emulator.Types               (Wallet (..))
main :: IO ()
main = void $
  Simulator.runSimulationWith handlers $ do
    logString @(Builtin UniswapContracts) "Starting Uniswap PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidInit <- Simulator.activateContract (Wallet 1) UniswapInit
    cs <- flip Simulator.waitForState cidInit $ \json -> case fromJSON json of
      Success (Just (Semigroup.Last cur)) -> Just $ Currency.currencySymbol cur
      _                                   -> Nothing
    _ <- Simulator.waitUntilFinished cidInit

    logString @(Builtin UniswapContracts) $ "Initialization finished. Minted: " ++ show cs

    let coins = Map.fromList [(tn, Uniswap.mkCoin cs tn) | tn <- Uniswap.tokenNames]
        ada = Uniswap.mkCoin adaSymbol adaToken

    cidStart <- Simulator.activateContract (Wallet 1) UniswapOwnerContract
    _ <- Simulator.callEndpointOnInstance cidStart "start" (Uniswap.WithHistoryId "StartId" ())
    us <- flip Simulator.waitForState cidStart $ \json -> case (fromJSON json :: Result (WH.History (Either Text Uniswap.Uniswap))) of
      Success (WH.lookup "StartId" -> Just (Right us)) -> Just us
      _                                                -> Nothing
    logString @(Builtin UniswapContracts) $ "Uniswap instance created: " ++ show us

    cids <- fmap Map.fromList $
      forM Uniswap.wallets $ \w -> do
        cid <- Simulator.activateContract w $ UniswapUserContract us
        logString @(Builtin UniswapContracts) $ "Uniswap user contract started for " ++ show w
        Simulator.waitForEndpoint cid "funds"
        _ <- Simulator.callEndpointOnInstance cid "funds" (Uniswap.WithHistoryId "FundsId" ())
        v <- flip Simulator.waitForState cid $ \json -> case (fromJSON json :: Result (WH.History (Either Text Uniswap.UserContractState))) of
          Success (WH.lookup "FundsId" -> Just (Right v)) -> Just v
          _                                               -> Nothing
        logString @(Builtin UniswapContracts) $ "initial funds in wallet " ++ show w ++ ": " ++ show v
        return (w, cid)

    _ <- liftIO getLine
    shutdown

data UniswapContracts
  = UniswapOwnerContract
  | UniswapUserContract Uniswap.Uniswap
  | UniswapInit
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty UniswapContracts where
  pretty = viaShow

handleStarterContract ::
  ( Member (Error PABError) effs,
    Member (LogMsg (PABMultiAgentMsg (Builtin UniswapContracts))) effs
  ) =>
  ContractEffect (Builtin UniswapContracts)
    ~> Eff effs
handleStarterContract = Builtin.handleBuiltin getSchema getContract
  where
    getSchema = \case
      UniswapOwnerContract -> Builtin.endpointsToSchemas @(Uniswap.UniswapOwnerSchema)
      UniswapUserContract _ -> Builtin.endpointsToSchemas @(Uniswap.UniswapUserSchema)
      UniswapInit -> Builtin.endpointsToSchemas @Empty
    getContract = \case
      UniswapOwnerContract  -> SomeBuiltin Uniswap.ownerEndpoint
      UniswapUserContract u -> SomeBuiltin (Uniswap.userEndpoints u)
      UniswapInit           -> SomeBuiltin Uniswap.setupTokens

handlers :: SimulatorEffectHandlers (Builtin UniswapContracts)
handlers =
  Simulator.mkSimulatorHandlers @(Builtin UniswapContracts) [] $ -- [UniswapOwnerContract, UniswapUserContract]
    interpret handleStarterContract
