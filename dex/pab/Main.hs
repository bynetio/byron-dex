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

import           Data.Aeson                          (FromJSON (..), ToJSON (..))
import           Data.OpenApi.Internal.Schema        (ToSchema)
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import qualified Dex.OffChain                        as Dex
import qualified Dex.Trace                           as Trace
import           GHC.Generics                        (Generic)
import           Language.PureScript.Bridge          (argonaut, equal, genericShow, mkSumType)
import           Plutus.Contract                     (Empty)
import           Plutus.PAB.Effects.Contract.Builtin (SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Run                      (runWith)
import           Plutus.PAB.Run.PSGenerator          (HasPSTypes (..))


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

