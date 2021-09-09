module Uniswap.Component.HTML.AddLiquidityModal where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.RemoteData (RemoteData(..), fromMaybe)
import Type.Proxy (Proxy(..))
import Uniswap.Capability.Funds (class ManageFunds, getFunds)
import Uniswap.Capability.Pool (class ManagePool, addToLiquidityPool)
import Uniswap.Component.CoinInputPanel as CoinInputPanel
import Uniswap.Component.HTML.Utils (css, maybeElem, whenElem)
import Uniswap.Data.Amount (Amount)
import Uniswap.Data.Coin (Coin)
import Uniswap.Data.Fee (Fee)
import Uniswap.Data.Funds (Fund)
import Uniswap.Data.LiquidityPool (LiquidityPool)
import Uniswap.Form.Field as Field
import Uniswap.Form.Validation as V
import Web.Event.Event as Event

-- Modal comonent to add liquidity
data Action
  = Initialize
  | HandleAddLiquidity LiquidityPool
  | CloseModal
  | LoadFunds

data Query a
  = ShowModal Pool a

type Pool
  = { coinA :: Coin
    , coinB :: Coin
    , fee :: Fee
    }

type State
  = { pool :: Maybe Pool
    , funds :: RemoteData String (Array Fund)
    }

type ChildSlots
  = ( addLiquidityForm :: FormSlot Unit )

modalProxy = Proxy :: Proxy "addLiquidityModal"

component ::
  forall o m.
  MonadAff m =>
  ManagePool m =>
  ManageFunds m =>
  H.Component Query Unit o m
component =
  H.mkComponent
    { initialState: \_ -> { funds: NotAsked, pool: Nothing }
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
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
    HandleAddLiquidity form -> do
      addToLiquidityPool form
      H.modify_ _ { pool = Nothing }
    CloseModal -> H.modify_ _ { pool = Nothing }

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    ShowModal pool a -> do
      H.modify_ _ { pool = Just pool }
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { funds, pool } = case funds of
    NotAsked -> text "Funds not loaded"
    Loading -> text "Loading..."
    Failure err -> text ("Error during loadinig wallet funds: " <> err)
    Success walletFunds -> maybeElem pool \p -> renderModal walletFunds p
    where
    text str = HH.div_ [ HH.text str ]

  renderModal walletFunds pool =
    HH.div
      [ css "modal is-active" ]
      [ HH.div [ css "modal-background" ] []
      , HH.div
          [ css "modal-card" ]
          [ HH.header
              [ css "modal-card-head" ]
              [ HH.p
                  [ css "modal-card-title" ]
                  [ HH.text "Add Liquidity" ]
              , HH.button
                  [ css "delete"
                  , HE.onClick \_ -> CloseModal
                  ]
                  []
              ]
          , renderForm
          ]
      ]
    where
    renderForm = do
      let
        form = formComponent pool walletFunds
      HH.slot _addLiquidityForm unit form unit HandleAddLiquidity

-- Form types
newtype AddLiquidityForm (r :: Row Type -> Type) f
  = AddLiquidityForm (r (AddLiquidityRow f))

derive instance newtypeAddLiquidityForm :: Newtype (AddLiquidityForm r f) _

type AddLiquidityRow :: (Type -> Type -> Type -> Type) -> Row Type
type AddLiquidityRow f
  = ( coinA :: f V.FormError (Maybe Coin) Coin
    , coinB :: f V.FormError (Maybe Coin) Coin
    , amountA :: f V.FormError (Maybe Amount) Amount
    , amountB :: f V.FormError (Maybe Amount) Amount
    , fee :: f V.FormError (Maybe Fee) Fee
    )

-- Form component types
type FormSlot
  = H.Slot (F.Query AddLiquidityForm FormQuery FormChildSlots) LiquidityPool

type FormChildSlots
  = ( coinInputPanel :: H.Slot CoinInputPanel.Query CoinInputPanel.Message CoinSlot )

_addLiquidityForm = Proxy :: Proxy "addLiquidityForm"

data CoinSlot
  = CoinA
  | CoinB

derive instance eqCoinSlot :: Eq CoinSlot

derive instance ordCoinSlot :: Ord CoinSlot

type FormState
  = ( formError :: Boolean )

data FormAction
  = Submit Event.Event
  | HandleCoinInputPanel CoinSlot CoinInputPanel.Message

data FormQuery a
  = SetFormError Boolean a

derive instance functorFormQuery :: Functor FormQuery

prx :: F.SProxies AddLiquidityForm
prx = F.mkSProxies (Proxy :: Proxy AddLiquidityForm)

formComponent ::
  forall i m.
  MonadAff m =>
  Pool ->
  (Array Fund) ->
  F.Component AddLiquidityForm FormQuery FormChildSlots i LiquidityPool m
formComponent { coinA, coinB, fee } _ =  -- use funds to validatie amount to avoid invest over user funds
  F.component input
    $ F.defaultSpec
        { render = render
        , handleAction = handleAction
        , handleEvent = handleEvent
        , handleQuery = handleQuery
        }
  where
  input :: i -> F.Input AddLiquidityForm FormState m
  input _ =
    { validators:
        AddLiquidityForm
          { coinA: V.exists
          , amountA: V.exists
          , coinB: V.exists
          , amountB: V.exists
          , fee: V.exists
          }
    , initialInputs: Just (F.wrapInputFields { coinA: Just coinA, coinB: Just coinB, amountA: Nothing, amountB: Nothing, fee: Just fee })
    , formError: false
    }

  eval act = F.handleAction handleAction handleEvent act

  handleEvent = F.raiseResult

  handleQuery ::
    forall a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetFormError bool a -> do
      H.modify_ _ { formError = bool }
      pure (Just a)

  handleAction = case _ of
    Submit event -> do
      H.liftEffect $ Event.preventDefault event
      eval F.submit
    HandleCoinInputPanel slot message -> case slot of
      CoinA -> case message of
        CoinInputPanel.Selected coin -> eval $ F.setValidate prx.coinA (Just coin)
        CoinInputPanel.Updated amount -> eval $ F.setValidate prx.amountA (Just amount)
        CoinInputPanel.Cleared -> do
          eval $ F.setValidate prx.coinA Nothing
          eval $ F.setValidate prx.amountA Nothing
      CoinB -> case message of
        CoinInputPanel.Selected coin -> eval $ F.setValidate prx.coinB (Just coin)
        CoinInputPanel.Updated amount -> eval $ F.setValidate prx.amountB (Just amount)
        CoinInputPanel.Cleared -> do
          eval $ F.setValidate prx.coinB Nothing
          eval $ F.setValidate prx.amountB Nothing

  render { formError } =
    HH.section
      [ css "modal-card-body" ]
      [ HH.form
          [ HE.onSubmit \ev -> F.injAction $ Submit ev ]
          [ whenElem formError \_ ->
              HH.div
                [ css "error-messages" ]
                [ HH.text "all fields are required" ]
          , HH.div
              [ css " is-rounded" ]
              [ HH.h1
                  [ css "label" ]
                  [ HH.text "Deposit Amounts" ]
              , HH.div
                  [ css "card-content" ]
                  [ HH.div
                      [ css "content" ]
                      [ coinInput CoinA coinA
                      , coinInput CoinB coinB
                      ]
                  ]
              , Field.submit "Add!"
              ]
          ]
      ]
    where
    coinInput slot coin = do
      HH.slot CoinInputPanel._coinInputPanel slot CoinInputPanel.coinInputPanel cipInput handler
      where
      handler = F.injAction <<< HandleCoinInputPanel slot

      cipInput =
        { coin: Just coin
        , amount: Nothing
        , label: ""
        , coins: []
        }
