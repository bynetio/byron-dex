module Uniswap.Page.LiquidityPools where

import Prelude
import Data.Array (mapWithIndex)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Network.RemoteData (RemoteData(..), fromMaybe)
import Uniswap.Capability.Funds (class ManageFunds, getFunds)
import Uniswap.Capability.Navigate (class Navigate)
import Uniswap.Capability.Pool (class ManagePool, getLiquidityPools)
import Uniswap.Capability.Timer (class Timer, timer)
import Uniswap.Component.CoinInputPanel as CoinInputPanel
import Uniswap.Component.HTML.AddLiquidityModal as Modal
import Uniswap.Component.HTML.Header (header)
import Uniswap.Component.HTML.Utils (css, loadRemoteData, safeHref)
import Uniswap.Data.Amount as Amount
import Uniswap.Data.Coin as Coin
import Uniswap.Data.Fee as Fee
import Uniswap.Data.Funds (Fund)
import Uniswap.Data.LiquidityPool (LiquidityPoolView)
import Uniswap.Data.Route as Route
import Uniswap.Data.Wallet (Wallet)
import Uniswap.Store as Store

data Action
  = Initialize
  | Receive (Connected (Maybe Wallet) Unit)
  | LoadLiquidityPools
  | RefreshLiquidityPools
  | LoadFunds
  | RefreshFunds
  | Refresh
  | ShowModal Modal.OperationType LiquidityPoolView

type State
  = { currentWallet :: Maybe Wallet
    , pools :: RemoteData String (Array LiquidityPoolView)
    , funds :: RemoteData String (Array Fund)
    }

type Slots
  = ( coinInputPanel :: forall query. H.Slot query CoinInputPanel.Message Int
    , manageLiquidityModal :: forall output. H.Slot Modal.Query output Unit
    )

type Resources
  = { pools :: Array LiquidityPoolView
    , funds :: Array Fund
    }

component ::
  forall q o m.
  MonadAff m =>
  MonadStore Store.Action Store.Store m =>
  Navigate m =>
  ManagePool m =>
  ManageFunds m =>
  Timer m =>
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
    { pools: NotAsked
    , funds: NotAsked
    , currentWallet
    }

  handleAction :: Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork $ handleAction $ LoadLiquidityPools
      void $ H.fork $ handleAction $ LoadFunds
      _ <- H.subscribe =<< timer Refresh
      pure unit
    Receive { context: currentWallet } -> H.modify_ _ { currentWallet = currentWallet }
    LoadLiquidityPools -> do
      H.modify_ _ { pools = Loading }
      pools <- getLiquidityPools
      H.modify_ _ { pools = fromMaybe pools }
    RefreshLiquidityPools -> do
      pools <- getLiquidityPools
      H.modify_ _ { pools = fromMaybe pools }
    LoadFunds -> do
      H.modify_ _ { funds = Loading }
      funds <- getFunds
      H.modify_ _ { funds = fromMaybe funds }
    RefreshFunds -> do
      funds <- getFunds
      H.modify_ _ { funds = fromMaybe funds }
    Refresh -> do
      void $ H.fork $ handleAction $ RefreshLiquidityPools
      void $ H.fork $ handleAction $ RefreshFunds
    ShowModal op { coinA, coinB, fee } -> do
      let
        cmd = Modal.ShowModal op { coinA, coinB, fee }
      H.tell Modal.modalProxy unit cmd

  render :: State -> H.ComponentHTML Action Slots m
  render { pools, funds, currentWallet } =
    container \res ->
      [ poolTable res
      , HH.a
          [ css "button is-info"
          , safeHref $ Route.AddPool
          ]
          [ HH.i_ []
          , HH.text "Create a Pool"
          ]
      , HH.slot_ Modal.modalProxy unit Modal.component unit
      ]
    where
    container html =
      HH.div_
        [ header currentWallet Route.Pools
        , HH.div
            [ css "container page" ]
            [ HH.div
                [ css "box columns is-centered" ]
                [ HH.div
                    [ css "column" ]
                    (renderHtml html)
                ]
            ]
        ]

    renderHtml html = case loadRemoteData { pools, funds } of
      Left err -> [ err ]
      Right rd -> html rd

poolTable ::
  forall props.
  Resources ->
  HH.HTML props Action
poolTable res =
  HH.table
    [ css "table is-striped is-fullwidth is-hoverable" ]
    [ HH.thead_
        [ HH.tr_
            [ HH.th_ [ HH.text "#" ]
            , HH.th_ [ HH.text "Pool Name" ]
            , HH.th_ [ HH.text "Amount A" ]
            , HH.th_ [ HH.text "Amount B" ]
            , HH.th_ [ HH.text "Fee" ]
            , HH.th_ [ HH.text "" ]
            ]
        ]
    , HH.tbody_ (row `mapWithIndex` res.pools)
    ]
  where
  row :: forall p. Int -> LiquidityPoolView -> HH.HTML p Action
  row idx lp@{ coinA, coinB, amountA, amountB, fee } =
    HH.tr_
      [ HH.th_ [ HH.text (show (idx + 1)) ]
      , HH.td_ [ HH.text $ Coin.toTokenNameString coinA <> "/" <> Coin.toTokenNameString coinB ]
      , HH.td_ [ HH.text (Amount.toString amountA) ]
      , HH.td_ [ HH.text (Amount.toString amountB) ]
      , HH.td_ [ HH.text (Fee.toString fee) ]
      , HH.td_
          [ HH.div_
              [ HH.a
                  [ css "button", HE.onClick \_ -> ShowModal Modal.AddLiquidity lp ]
                  [ HH.text "Add" ]
              , HH.a
                  [ css "button", HE.onClick \_ -> ShowModal Modal.RemoveLiquidity lp ]
                  [ HH.text "Remove" ]
              ]
          ]
      ]
