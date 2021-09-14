module Uniswap.AppM where

import Prelude
import Control.Monad.Rec.Class (forever)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut as Codec
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT)
import Halogen.Subscription as HS
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Safe.Coerce (coerce)
import Uniswap.Api.Endpoint as Endpoint
import Uniswap.Api.Request (RequestMethod(..))
import Uniswap.Api.Utils (mkRequest, decode)
import Uniswap.Capability.Funds (class ManageFunds)
import Uniswap.Capability.Logger (class Logger)
import Uniswap.Capability.Navigate (class Navigate)
import Uniswap.Capability.Now (class Now)
import Uniswap.Capability.Pool (class ManagePool)
import Uniswap.Capability.Swap (class ManageSwap)
import Uniswap.Capability.Timer (class Timer)
import Uniswap.Data.Funds as F
import Uniswap.Data.LiquidityPool as LP
import Uniswap.Data.Log as Log
import Uniswap.Data.Route as Route
import Uniswap.Data.Swap as Swap
import Uniswap.Store (Action, Store)
import Uniswap.Store as Store

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
    mbJson <- mkRequest { endpoint: Endpoint.Pools, method: Get }
    decode (CA.array LP.codecLiquidityPool) mbJson
  createLiquidityPool body = void $ mkRequest { endpoint: Endpoint.CreatePool, method: mkPost LP.codecLiquidityPool body }
  closeLiquidityPool body = void $ mkRequest { endpoint: Endpoint.ClosePool, method: mkPost LP.codecCloseLiquidityPool body }
  addToLiquidityPool body = void $ mkRequest { endpoint: Endpoint.AddToPool, method: mkPost LP.codecLiquidityPool body }
  removeFromLiquidityPool body = void $ mkRequest { endpoint: Endpoint.RemoveFromPool, method: mkPost LP.codecRemoveLiquidityPool body }

instance swapPoolAppM :: ManageSwap AppM where
  swap body = void $ mkRequest { endpoint: Endpoint.Swap, method: mkPost Swap.swapCodec body }
  swapPreview body = void $ mkRequest { endpoint: Endpoint.SwapPreview, method: mkPost Swap.swapPreviewCodec body }
  indirectSwap body = void $ mkRequest { endpoint: Endpoint.IndirectSwap, method: mkPost Swap.iSwapCodec body }
  indirectSwapPreview body = do
    mbJson <- mkRequest { endpoint: Endpoint.IndirectSwapPreview, method: mkPost Swap.iSwapPreviewCodec body }
    decode Swap.swapPreviewResponseCodec mbJson

mkPost codec body = Post $ Just $ Codec.encode codec body

instance manageFundsAppM :: ManageFunds AppM where
  getFunds = do
    mbJson <- mkRequest { endpoint: Endpoint.Funds, method: Get }
    decode (CA.array F.codec) mbJson

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

instance loggerAppM :: Logger AppM where
  logger log = do
    -- { logLevel } <- getStore
    liftEffect $ Console.log $ Log.message log

instance timerAppM :: Timer AppM where
  timer val = do
    { emitter, listener } <- liftEffect HS.create
    _ <-
      H.liftAff $ Aff.forkAff
        $ forever do
            Aff.delay $ Aff.Milliseconds 5000.0 -- 5 seconds
            liftEffect $ HS.notify listener val
    pure emitter
