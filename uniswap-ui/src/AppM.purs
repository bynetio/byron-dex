module Uniswap.AppM where

import Prelude
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut as Codec
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now as Now
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT)
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Safe.Coerce (coerce)
import Uniswap.Api.Endpoint (Endpoint(..))
import Uniswap.Api.Request (RequestMethod(..))
import Uniswap.Api.Utils (mkRequest, decode)
import Uniswap.Capability.Navigate (class Navigate)
import Uniswap.Capability.Now (class Now)
import Uniswap.Capability.Pool (class ManagePool)
import Uniswap.Capability.Logger (class Logger)
import Uniswap.Data.LiquidityPool as LP
import Uniswap.Data.Route as Route
import Uniswap.Data.Log as Log
import Uniswap.Store (Action, Store)
import Uniswap.Store as Store
import Effect.Console as Console

newtype AppM a
  = AppM (StoreT Store.Action Store.Store Aff a)

runAppM :: forall q i o. Store.Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

derive newtype instance monadStoreAppM :: MonadStore Action Store AppM

instance navigateAppM :: Navigate AppM where
  navigate = liftEffect <<< setHash <<< print Route.routeCodec

instance managePoolAppM :: ManagePool AppM where
  getLiquidityPools = do
    mbJson <- mkRequest { endpoint: Pools, method: Get }
    decode (CA.array LP.codec) mbJson
  createLiquidityPool body =
    let
      method = Post $ Just $ Codec.encode LP.codec body
    in
      void $ mkRequest { endpoint: CreatePool, method }

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

instance loggerAppM :: Logger AppM where
  logger log = do
    -- { logLevel } <- getStore
    liftEffect $ Console.log $ Log.message log
