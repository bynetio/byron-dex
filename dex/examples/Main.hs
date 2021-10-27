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

module Main (main) where

import           Control.Monad                       (forM, void)
import           Control.Monad.Freer                 (interpret)
import           Data.Aeson                          (FromJSON (..),
                                                      ToJSON (..), Value)
import           Data.Aeson.Types                    (parseMaybe)
import           Data.Default                        (Default (def))
import qualified Data.Map                            as Map
import           Data.OpenApi.Internal.Schema        (ToSchema)
import qualified Data.Semigroup                      as Semigroup
import           Data.Text                           (Text)
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import qualified Dex.OffChain                        as Dex
import qualified Dex.Trace                           as Trace
import           Dex.Types                           (DexContractState)
import qualified Dex.Types                           as Dex
import qualified Dex.WalletHistory                   as WH
import           GHC.Generics                        (Generic)
import           Plutus.Contract                     (Empty)
import qualified Plutus.Contracts.Currency           as Currency
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (Simulation,
                                                      SimulatorEffectHandlers,
                                                      logString)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.V1.Ledger.Value              as Value
import           Wallet.Emulator.Types               (knownWallet)
import           Wallet.Types                        (ContractInstanceId)

type ContractHistory = WH.History (Either Text Dex.DexContractState)

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
    DexContract -> SomeBuiltin Dex.dexEndpoints
    DexInit     -> SomeBuiltin Trace.setupTokens

handlers :: SimulatorEffectHandlers (Builtin DexContracts)
handlers = Simulator.mkSimulatorHandlers def def $
  interpret (Builtin.contractHandler (Builtin.handleBuiltin @DexContracts))

main :: IO ()
main = void $
  Simulator.runSimulationWith handlers $ do
    cidInit <- Simulator.activateContract (knownWallet 1) DexInit
    cs <- getState (Currency.currencySymbol . Semigroup.getLast) cidInit

    void $ Simulator.waitUntilFinished cidInit

    logString @(Builtin DexContracts) $ "Initialization finished. Minted: " ++ show cs

    void $ fmap Map.fromList $
      forM Trace.wallets $ \w -> do
        cid <- Simulator.activateContract w DexContract
        logString @(Builtin DexContracts) $ "Uniswap user contract started for " ++ show w
        Simulator.waitForEndpoint cid "funds"
        void $ Simulator.callEndpointOnInstance cid "funds" (Dex.Request "FundsId" 0 ())
        v <- getState (contractState "FundsId") cid
        logString @(Builtin DexContracts) $ "initial funds in wallet " ++ show w ++ ": " ++ show v
        return (w, cid)

    let coins = fmap (Value.assetClass cs) Trace.tokenNames
    let w1 = head Trace.wallets
    let w2 = Trace.wallets !! 1
    let a  = head coins
    let b  = coins !! 1

    logString @(Builtin DexContracts) $ ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> SWAPS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
    logString @(Builtin DexContracts) $ "Use case Nº1: Simple swap"
    w1cid <- Simulator.activateContract w1 DexContract
    w2cid <- Simulator.activateContract w2 DexContract

    launchEndpoint w1cid "createSellOrder" (Dex.Request "firstOrder"   1 $ Dex.SellOrderParams a b 100 100) $ "* 100A <-> 100B sell order from wallet " ++ show w1
    launchEndpoint w2cid "createSellOrder" (Dex.Request "secondOrder"  2 $ Dex.SellOrderParams b a 100 100) $ "* 100B <-> 100A sell order from wallet " ++ show w2
    launchEndpoint w1cid "perform"         (Dex.Request "perform"      3 ())                                  "* perform"
    launchEndpoint w1cid "collectFunds"    (Dex.Request "collectFunds" 3 ())                                  "* collect funds"
    launchEndpoint w1cid "funds"           (Dex.Request "funds"        4 ())                                $ "* funds of " ++ show w1
    launchEndpoint w2cid "funds"           (Dex.Request "funds"        5 ())                                $ "* funds of " ++ show w2

    logString @(Builtin DexContracts) $ ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> DONE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"

  where
    launchEndpoint cid endpointName endpointParams description = do
      logString @(Builtin DexContracts) description
      Simulator.waitForEndpoint cid endpointName
      void $ Simulator.callEndpointOnInstance cid endpointName endpointParams
      let paramsId = Dex.historyId endpointParams
      result <- getState (contractState paramsId) cid
      logString @(Builtin DexContracts) $ "  result: " ++ show result

    fromJSONValue :: FromJSON a => Value -> Maybe a
    fromJSONValue = parseMaybe parseJSON

    getState :: (FromJSON a) => (a -> b) -> ContractInstanceId -> Simulation t b
    getState f = Simulator.waitForState (fmap f . fromJSONValue)

    contractState :: Text -> ContractHistory -> Maybe DexContractState
    contractState historyId (WH.lookup historyId -> Just (Right v)) = Just v
    contractState _ _                                               = Nothing
