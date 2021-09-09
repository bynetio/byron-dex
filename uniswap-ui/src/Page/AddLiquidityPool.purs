module Uniswap.Page.AddLiquidityPool where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Network.RemoteData (RemoteData(..), fromMaybe)
import Select as Select
import Type.Proxy (Proxy(..))
import Uniswap.Capability.Funds (class ManageFunds, getFunds)
import Uniswap.Capability.Navigate (class Navigate, navigate)
import Uniswap.Capability.Pool (class ManagePool, createLiquidityPool)
import Uniswap.Component.Dropdown as DD
import Uniswap.Component.HTML.Header (header)
import Uniswap.Component.HTML.Utils (css, whenElem)
import Uniswap.Data.Amount (Amount)
import Uniswap.Data.Coin (Coin)
import Uniswap.Data.Fee (Fee)
import Uniswap.Data.Funds (Fund)
import Uniswap.Data.LiquidityPool (LiquidityPool)
import Uniswap.Data.Route (Route(..))
import Uniswap.Data.Wallet (Wallet)
import Uniswap.Form.Field as Field
import Uniswap.Form.Validation as V
import Uniswap.Store as Store
import Web.Event.Event as Event

data Action
  = Initialize
  | HandleCreateForm LiquidityPoolFields
  | LoadFunds

type State
  = { currentWallet :: Maybe Wallet
    , funds :: RemoteData String (Array Fund)
    }

type ChildSlots
  = ( formless :: F.Slot AddPoolForm FormQuery ChildSlotsForm LiquidityPoolFields Unit )

type LiquidityPoolFields
  = { coinA :: Coin
    , amountA :: Amount
    , coinB :: Coin
    , amountB :: Amount
    , fee :: Fee
    }

component ::
  forall q o m.
  MonadAff m =>
  MonadStore Store.Action Store.Store m =>
  Navigate m =>
  ManagePool m =>
  ManageFunds m =>
  H.Component q Unit o m
component =
  connect (selectEq _.currentWallet)
    $ H.mkComponent
        { initialState:
            \{ context: currentWallet } ->
              { currentWallet
              , funds: NotAsked
              }
        , render
        , eval:
            H.mkEval
              $ H.defaultEval
                  { handleAction = handleAction
                  , initialize = Just Initialize
                  }
        }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> void $ H.fork $ handleAction $ LoadFunds
    LoadFunds -> do
      H.modify_ _ { funds = Loading }
      funds <- getFunds
      H.modify_ _ { funds = fromMaybe funds }
    HandleCreateForm { coinA, amountA, coinB, amountB, fee } -> do
      let
        lp :: LiquidityPool
        lp =
          { coinA: coinA
          , amountA: amountA
          , coinB: coinB
          , amountB: amountB
          , fee: fee
          }
      createLiquidityPool lp
        *> do
            void $ H.query F._formless unit $ F.injQuery $ SetFormError false unit
            navigate Pools

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { currentWallet, funds } =
    container
      [ HH.h1
          [ css "label" ]
          [ HH.text "Create a Pool" ]
      , renderForm funds
      ]
    where
    container html =
      HH.div_
        [ header currentWallet AddPool
        , HH.div
            [ css "" ]
            [ HH.div
                [ css "container" ]
                [ HH.div
                    [ css "box columns is-centered" ]
                    [ HH.div
                        [ css "column" ]
                        html
                    ]
                ]
            ]
        ]

    renderForm = case _ of
      NotAsked -> HH.text "Wallet funds not loaded"
      Loading -> HH.text "Loading wallet funds..."
      Failure err -> HH.text ("Error during loading wallets funds: " <> err)
      Success walletFunds -> do
        let
          form = formComponent walletFunds
        HH.slot F._formless unit form unit HandleCreateForm

newtype AddPoolForm (r :: Row Type -> Type) f
  = AddPoolForm
  ( r
      ( coinA :: f V.FormError (Maybe Coin) Coin
      , coinB :: f V.FormError (Maybe Coin) Coin
      , amountA :: f V.FormError String Amount
      , amountB :: f V.FormError String Amount
      , fee :: f V.FormError String Fee
      )
  )

derive instance newtypeAddPoolFields :: Newtype (AddPoolForm r f) _

data FormAction
  = Submit Event.Event
  | HandleCoin DDSlot (DD.Message Coin)

data FormQuery a
  = SetFormError Boolean a

derive instance functorFormQuery :: Functor FormQuery

type ChildSlotsForm
  = ( dropdown :: DD.Slot Coin DDSlot )

data DDSlot
  = CoinA
  | CoinB

derive instance eqDDSlot :: Eq DDSlot

derive instance ordDDSlot :: Ord DDSlot

formComponent ::
  forall i m.
  MonadAff m =>
  (Array Fund) ->
  F.Component AddPoolForm FormQuery ChildSlotsForm i LiquidityPoolFields m
formComponent funds =
  F.component formInput
    $ F.defaultSpec
        { render = renderCreateLP
        , handleEvent = handleEvent
        , handleQuery = handleQuery
        , handleAction = handleAction
        }
  where
  formInput :: i -> F.Input AddPoolForm ( formError :: Boolean ) m
  formInput _ =
    { validators:
        AddPoolForm
          { coinA: V.exists
          , amountA: V.required >>> V.amountFormat
          , coinB: V.exists
          , amountB: V.required >>> V.amountFormat
          , fee: V.required >>> V.feeFormat
          }
    , initialInputs: Nothing
    , formError: false
    }

  handleEvent = F.raiseResult

  handleAction = case _ of
    Submit event -> do
      H.liftEffect $ Event.preventDefault event
      eval F.submit
    HandleCoin slot message -> case slot of
      CoinA -> case message of
        DD.Selected coin -> eval $ F.setValidate proxies.coinA (Just coin)
        DD.Cleared -> eval $ F.setValidate proxies.coinA Nothing
      CoinB -> case message of
        DD.Selected coin -> eval $ F.setValidate proxies.coinB (Just coin)
        DD.Cleared -> eval $ F.setValidate proxies.coinB Nothing
    where
    eval act = F.handleAction handleAction handleEvent act

  handleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetFormError bool a -> do
      H.modify_ _ { formError = bool }
      pure (Just a)

  proxies = F.mkSProxies (Proxy :: _ AddPoolForm)

  renderCoin help slot label =
    Field.field
      { label: label
      , help: help
      }
      [ HH.slot DD._dropdown slot (Select.component DD.input DD.spec) coinsInput handler ]
    where
    handler = F.injAction <<< (HandleCoin slot)

    coinsInput =
      { placeholder: "Choose " <> label
      , item: Nothing
      , items: map (\f -> f.coin) funds
      }

  renderCreateLP { form, formError } =
    HH.form
      [ HE.onSubmit \ev -> F.injAction $ Submit ev ]
      [ whenElem formError \_ ->
          HH.div
            [ css "error-messages" ]
            [ HH.text "all fields are required" ]
      , HH.fieldset_
          [ renderCoin (F.getResult proxies.coinA form # V.resultToHelp "") CoinA "Coin A"
          , Field.input'
              { label: "Amount A"
              , help: F.getResult proxies.amountA form # V.resultToHelp ""
              , placeholder: "Amount"
              , inputType: HP.InputText
              }
              [ HP.value $ F.getInput proxies.amountA form
              , HE.onValueInput (F.setValidate proxies.amountA)
              ]
          , renderCoin (F.getResult proxies.coinA form # V.resultToHelp "") CoinB "Coin B"
          , Field.input'
              { label: "Amount B"
              , help: F.getResult proxies.amountA form # V.resultToHelp ""
              , placeholder: "Amount"
              , inputType: HP.InputText
              }
              [ HP.value $ F.getInput proxies.amountB form
              , HE.onValueInput (F.setValidate proxies.amountB)
              ]
          , Field.input'
              { label: "Fee"
              , help: F.getResult proxies.fee form # V.resultToHelp ""
              , placeholder: "Fee"
              , inputType: HP.InputText
              }
              [ HP.value $ F.getInput proxies.fee form
              , HE.onValueInput (F.setValidate proxies.fee)
              ]
          , Field.submit "Create!"
          ]
      ]
