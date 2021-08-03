module Uniswap.Page.LiquidityPools where

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
import Uniswap.Capability.Navigate (class Navigate)
import Uniswap.Capability.Pool (class ManagePool, getLiquidityPools)
import Uniswap.Component.HTML.Header (header)
import Uniswap.Component.HTML.Utils (css, safeHref)
import Uniswap.Data.Amount as Amount
import Uniswap.Data.LiquidityPool (LiquidityPool)
import Uniswap.Data.Route (Route(..))
import Uniswap.Data.Wallet (Wallet)
import Uniswap.Data.Fee as Fee
import Uniswap.Store as Store

data Action
  = Initialize
  | Receive (Connected (Maybe Wallet) Unit)
  | LoadLiquidityPools

type State
  = { currentWallet :: Maybe Wallet
    , liquidityPools :: RemoteData String (Array LiquidityPool)
    }

component ::
  forall q o m.
  MonadAff m =>
  MonadStore Store.Action Store.Store m =>
  Navigate m =>
  ManagePool m =>
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
    { liquidityPools: NotAsked
    , currentWallet
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> void $ H.fork $ handleAction $ LoadLiquidityPools
    Receive { context: currentWallet } -> H.modify_ _ { currentWallet = currentWallet }
    LoadLiquidityPools -> do
      H.modify_ _ { liquidityPools = Loading }
      pools <- getLiquidityPools
      H.modify_ _ { liquidityPools = fromMaybe pools }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render state =
    container
      [ HH.table
          [ css "table table-striped" ]
          [ HH.thead
              []
              [ HH.tr
                  []
                  [ HH.th
                      []
                      [ HH.text "#" ]
                  , HH.th
                      []
                      [ HH.text "Pool Name" ]
                  , HH.th
                      []
                      [ HH.text "Amount A" ]
                  , HH.th
                      []
                      [ HH.text "Amount B" ]
                  , HH.th
                      []
                      [ HH.text "Fee" ]
                  ]
              ]
          , poolList state.liquidityPools
          ]
      , HH.a
          [ css "btn btn-outline-primary btn-sm"
          , safeHref $ AddPool
          ]
          [ HH.i
              [ css "ion-edit" ]
              []
          , HH.text "Create a Pool"
          ]
      ]
    where
    container html =
      HH.div
        [ css "" ]
        [ header state.currentWallet Pools
        , HH.div
            [ css "container page" ]
            [ HH.div
                [ css "row" ]
                [ HH.div
                    [ css "col-md-12 col-xs12" ]
                    html
                ]
            ]
        ]

poolList ::
  forall props action.
  RemoteData String (Array LiquidityPool) ->
  HH.HTML props action
poolList = case _ of
  NotAsked -> text "Liquidity Pools not loaded"
  Loading -> text "Loading..."
  Failure err -> text ("Error during loading liquidity pools: " <> err)
  Success pools -> HH.tbody_ (row `mapWithIndex` pools)
  where
  text str =
    HH.div
      [ css "" ]
      [ HH.text str ]

  row :: forall p a. Int -> LiquidityPool -> HH.HTML p a
  row idx { coinA, coinB, amountA, amountB, fee } =
    HH.tr
      []
      [ HH.th [] [ HH.text (show (idx + 1)) ]
      , HH.td [] [ HH.text $ (show coinA.tokenName) <> "/" <> (show coinB.tokenName) ]
      , HH.td [] [ HH.text (Amount.toString amountA) ]
      , HH.td [] [ HH.text (Amount.toString amountB) ]
      , HH.td [] [ HH.text (Fee.toString fee) ]
      ]
