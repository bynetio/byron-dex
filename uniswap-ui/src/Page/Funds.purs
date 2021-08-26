module Uniswap.Page.Funds where

import Prelude
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Network.RemoteData (RemoteData(..), fromMaybe)
import Uniswap.Capability.Funds (class ManageFunds, getFunds)
import Uniswap.Component.HTML.Header (header)
import Uniswap.Component.HTML.Utils (css)
import Uniswap.Data.Funds (Fund)
import Uniswap.Data.Route (Route(..))
import Uniswap.Data.Wallet (Wallet)
import Uniswap.Data.Amount as Amount
import Uniswap.Data.Coin as Coin
import Uniswap.Store as Store

data Action
  = Initialize
  | Receive (Connected (Maybe Wallet) Unit)
  | LoadFunds

type State
  = { currentWallet :: Maybe Wallet
    , funds :: RemoteData String (Array Fund)
    }

component ::
  forall q o m.
  MonadAff m =>
  MonadStore Store.Action Store.Store m =>
  ManageFunds m =>
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
    { funds: NotAsked
    , currentWallet
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> void $ H.fork $ handleAction $ LoadFunds
    Receive { context: currentWallet } -> H.modify_ _ { currentWallet = currentWallet }
    LoadFunds -> do
      H.modify_ _ { funds = Loading }
      funds <- getFunds
      H.modify_ _ { funds = fromMaybe funds }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render state = container [ fundsTable state.funds ]
    where
    container html =
      HH.div
        []
        [ header state.currentWallet Funds
        , HH.div
            [ css "container" ]
            [ HH.div
                [ css "box columns is-centered" ]
                [ HH.div
                    [ css "column" ]
                    html
                ]
            ]
        ]

fundsTable ::
  forall props action.
  RemoteData String (Array Fund) ->
  HH.HTML props action
fundsTable = case _ of
  NotAsked -> text "Funds not loaded"
  Loading -> text "Loading..."
  Failure err -> text ("Error during loading wallet funds: " <> err)
  Success funds ->
    HH.table
      [ css "table is-striped is-fullwidth" ]
      [ HH.thead
          []
          [ HH.tr
              []
              [ HH.th
                  []
                  [ HH.text "#" ]
              , HH.th
                  []
                  [ HH.text "Token Name" ]
              , HH.th
                  []
                  [ HH.text "Currency Symbol" ]
              , HH.th
                  []
                  [ HH.text "Amount" ]
              ]
          ]
      , HH.tbody_ (row `mapWithIndex` funds)
      ]
  where
  text str =
    HH.div
      [ css "" ]
      [ HH.text str ]

  row :: forall p a. Int -> Fund -> HH.HTML p a
  row idx { coin, amount } =
    HH.tr
      []
      [ HH.th [] [ HH.text (show (idx + 1)) ]
      , HH.td [] [ HH.text (Coin.toTokenNameString coin) ]
      , HH.td [] [ HH.text (Coin.toCurrencySymbolString coin) ]
      , HH.td [] [ HH.text (Amount.toString amount) ]
      ]
