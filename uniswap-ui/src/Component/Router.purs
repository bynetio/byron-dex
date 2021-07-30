module Uniswap.Component.Router where

import Prelude
import Uniswap.Capability.Navigate (class Navigate, navigate)
import Uniswap.Data.Route (Route(..), routeCodec)
import Uniswap.Data.Wallet (Wallet)
import Uniswap.Store as Store
import Halogen as H
import Halogen (liftEffect)
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Data.Either (hush)
import App.Button as Button

type OpaqueSlot slot
  = forall query. H.Slot query Void slot

data Query a
  = Navigate Route a

type State
  = { route :: Maybe Route
    , currentWallet :: Maybe Wallet
    }

data Action
  = Initialize
  | Receive (Connected (Maybe Wallet) Unit)

type ChildSlots
  = ( home :: OpaqueSlot Unit
    , pools :: OpaqueSlot Unit
    )

component ::
  forall m.
  MonadAff m =>
  MonadStore Store.Action Store.Store m =>
  Navigate m =>
  H.Component Query Unit Void m
component =
  connect (selectEq _.currentWallet)
    $ H.mkComponent
        { initialState: \{ context: currentWallet } -> { route: Nothing, currentWallet }
        , render
        , eval:
            H.mkEval
              $ H.defaultEval
                  { handleQuery = handleQuery
                  , handleAction = handleAction
                  , receive = Just <<< Receive
                  , initialize = Just Initialize
                  }
        }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getHash
      navigate $ fromMaybe Home initialRoute
    Receive { context: currentWallet } -> H.modify_ _ { currentWallet = currentWallet }

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route } <- H.get
      -- do not rerender page if route is unchanged
      when (route /= Just dest) do
        H.modify_ _ { route = Just dest }
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route } = case route of
    Just r -> case r of
      Home -> HH.slot_ (Proxy :: _ "home") unit Button.component unit
      Pools -> HH.slot_ (Proxy :: _ "pools") unit Button.component { redirect: true }
    Nothing -> HH.div_ [ HH.text "Page Not Found" ]
