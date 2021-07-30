module Uniswap.AppM where

import Prelude
import Uniswap.Store as Store
import Uniswap.Store (Action, Store)
import Uniswap.Data.Route as Route
import Uniswap.Capability.Navigate (class Navigate)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT)
import Halogen as H
import Safe.Coerce (coerce)
import Routing.Duplex (print)
import Routing.Hash (setHash)

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
