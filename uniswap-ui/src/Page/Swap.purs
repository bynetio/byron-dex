module Uniswap.Page.Swap where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, getStore)
import Halogen.Store.Select (selectEq)
import Network.RemoteData (RemoteData(..), fromMaybe)
import Uniswap.Capability.Funds (class ManageFunds, getFunds)
import Uniswap.Capability.Navigate (class Navigate)
import Uniswap.Capability.Pool (class ManagePool, getLiquidityPools)
import Uniswap.Capability.Swap (class ManageSwap, indirectSwap)
import Uniswap.Component.HTML.Header (header)
import Uniswap.Component.HTML.SwapForm as Form
import Uniswap.Component.HTML.Utils (css)
import Uniswap.Data.Funds (Fund)
import Uniswap.Data.LiquidityPool (LiquidityPool)
import Uniswap.Data.Route as Route
import Uniswap.Data.Swap (SwapPreviewResponse)
import Uniswap.Data.Swap as Swap
import Uniswap.Data.Wallet (Wallet)
import Uniswap.Store as Store

data Action
  = Initialize
  | Receive (Connected (Maybe Wallet) Unit)
  | HandleSwapForm SwapPreviewResponse
  | LoadPools
  | LoadFunds

type State
  = { currentWallet :: Maybe Wallet
    , funds :: RemoteData String (Array Fund)
    , pools :: RemoteData String (Array LiquidityPool)
    }

type Slots
  = ( swapForm :: Form.Slot Unit
    )

component ::
  forall q o m.
  MonadAff m =>
  MonadStore Store.Action Store.Store m =>
  Navigate m =>
  ManagePool m =>
  ManageFunds m =>
  ManageSwap m =>
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
    , funds: NotAsked
    , pools: NotAsked
    }

  handleAction :: Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork $ handleAction $ LoadFunds
      void $ H.fork $ handleAction $ LoadPools
    Receive { context: currentWallet } -> H.modify_ _ { currentWallet = currentWallet }
    LoadFunds -> do
      H.modify_ _ { funds = Loading }
      funds <- getFunds
      H.modify_ _ { funds = fromMaybe funds }
    LoadPools -> do
      H.modify_ _ { pools = Loading }
      pools <- getLiquidityPools
      H.modify_ _ { pools = fromMaybe pools }
    HandleSwapForm preview -> do
      { slippageTolerance } <- getStore
      indirectSwap $ Swap.indirectOf preview slippageTolerance

  render :: State -> H.ComponentHTML Action Slots m
  render state@{ currentWallet } =
    container \({ pools, funds }) ->
      [ HH.div
          [ css "box swap-box" ]
          [ HH.h1
              [ css "label" ]
              [ HH.text "Swap"
              , HH.slot Form._swapForm unit (Form.component funds) unit HandleSwapForm
              ]
          ]
      ]
    where
    container html =
      HH.div_
        [ header currentWallet Route.Swap
        , HH.div
            [ css "container page" ]
            [ HH.div
                [ css "" ]
                [ HH.div
                    [ css "column" ]
                    renderHtml
                ]
            ]
        ]
      where
      renderHtml = case loadRemoteData of
        Left err -> [ err ]
        Right rd -> html rd

      loadRemoteData :: forall w i. Either (HH.HTML w i) { pools :: Array LiquidityPool, funds :: Array Fund }
      loadRemoteData = do
        pools <- load state.pools "pools"
        funds <- load state.funds "wallets funds"
        pure $ { pools, funds }

      load :: forall a w i. RemoteData String (Array a) -> String -> Either (HH.HTML w i) (Array a)
      load remoteData what = case remoteData of
        NotAsked -> Left $ HH.text (what <> " not loaded")
        Loading -> Left $ HH.text ("Loading  " <> what <> "...")
        Failure err -> Left $ HH.text ("Error during loading " <> what <> ": " <> err)
        Success xa -> Right xa
