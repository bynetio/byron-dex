{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
module Main
  ( main
  ) where

import           Control.Monad                       (forM, void)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON (..), ToJSON (..), Value)
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
import           Language.PureScript.Bridge          (argonaut, equal, genericShow, mkSumType)
import           Plutus.Contract                     (Empty)
import qualified Plutus.Contracts.Currency           as Currency
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Run                      (runWith)
import           Plutus.PAB.Run.PSGenerator          (HasPSTypes (..))
import           Plutus.PAB.Simulator                (Simulation, SimulatorEffectHandlers, logString)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Wallet.Emulator.Types               (knownWallet)
import           Wallet.Types                        (ContractInstanceId)

type ContractHistory = WH.History (Either Text Dex.DexContractState)




main :: IO ()
main = do
  runWith (Builtin.handleBuiltin @DexContracts)

data DexContracts = DexContract | DexInit deriving (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance Pretty DexContracts where
  pretty = viaShow

instance HasPSTypes DexContracts where
  psTypes =
    [ equal . genericShow . argonaut $ mkSumType @DexContracts
    ]

instance Builtin.HasDefinitions DexContracts where
    getDefinitions = [DexContract]
    getSchema = \case
      DexContract -> Builtin.endpointsToSchemas @Dex.DexSchema
      DexInit     -> Builtin.endpointsToSchemas @Empty
    getContract = \case
      DexContract -> SomeBuiltin Dex.dexEndpoints
      DexInit     -> SomeBuiltin Trace.setupTokens

handlers :: SimulatorEffectHandlers (Builtin DexContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (Builtin.contractHandler (Builtin.handleBuiltin @DexContracts))
