module Uniswap.Page.Swap where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Uniswap.Capability.Navigate (class Navigate)
import Uniswap.Component.HTML.Header (header)
import Uniswap.Component.HTML.Utils (css)
import Uniswap.Data.Route (Route(..))
import Uniswap.Data.Wallet (Wallet)
import Uniswap.Store as Store

data Action
  = Initialize
  | Receive (Connected (Maybe Wallet) Unit)

type State
  = { currentWallet :: Maybe Wallet }

component ::
  forall q o m.
  MonadAff m =>
  MonadStore Store.Action Store.Store m =>
  Navigate m =>
  H.Component q Unit o m
component =
  connect (selectEq _.currentWallet)
    $ H.mkComponent
        { initialState
        , render
        , eval:
            H.mkEval
              $ H.defaultEval
                  { handleAction = handleAction
                  , receive = Just <<< Receive
                  , initialize = Just Initialize
                  }
        }
  where
  initialState { context: currentWallet } =
    { currentWallet
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> pure unit
    Receive { context: currentWallet } -> H.modify_ _ { currentWallet = currentWallet }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render state = container []
    where
    container html =
      HH.div_
        [ header state.currentWallet Swap
        , HH.div
            [ css "container page" ]
            [ HH.div
                [ css "box columns is-centered" ]
                [ HH.div
                    [ css "column" ]
                    html
                ]
            ]
        ]
