module Uniswap.Component.HTML.AddLiquidityModal where  -- rename this component to ManageLiquidityModal

import Prelude
import Data.Either (either)
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
import Uniswap.Capability.Pool (class ManagePool, addToLiquidityPool, removeFromLiquidityPool)
import Uniswap.Component.CoinInputPanel as CoinInputPanel
import Uniswap.Component.HTML.Utils (css, load, toRight, whenElem)
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
  | HandleManageLiquidity LiquidityPool
  | CloseModal
  | LoadFunds

data OperationType
  = AddLiquidity
  | RemoveLiquidity

derive instance eqOperationType :: Eq OperationType

derive instance ordOperationType :: Ord OperationType

data Query a
  = ShowModal OperationType Pool a

type Pool
  = { coinA :: Coin
    , coinB :: Coin
    , fee :: Fee
    }

type State
  = { pool :: Maybe Pool
    , operation :: Maybe OperationType
    , funds :: RemoteData String (Array Fund)
    }

type ChildSlots
  = ( manageLiquidityForm :: FormSlot Unit )

modalProxy = Proxy :: Proxy "manageLiquidityModal"

component ::
  forall o m.
  MonadAff m =>
  ManagePool m =>
  ManageFunds m =>
  H.Component Query Unit o m
component =
  H.mkComponent
    { initialState: \_ -> { funds: NotAsked, pool: Nothing, operation: Nothing }
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
    HandleManageLiquidity form -> do
      { operation } <- H.get
      case operation of
        Just AddLiquidity -> addToLiquidityPool form
        Just RemoveLiquidity -> do
          removeFromLiquidityPool { coinA: form.coinA, coinB: form.coinB, fee: form.fee, diff: 254 }
        Nothing -> pure unit
      H.modify_ _ { pool = Nothing, operation = Nothing }
    CloseModal -> H.modify_ _ { pool = Nothing, operation = Nothing }

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    ShowModal operation pool a -> do
      H.modify_ _ { pool = Just pool, operation = Just operation }
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = do
    either identity identity renderEither
    where
    renderEither = do
      funds <- load state.funds "funds"
      operation <- toRight state.operation
      pool <- toRight state.pool
      pure $ renderModal funds pool operation

  renderModal walletFunds pool operation =
    HH.div
      [ css "modal is-active" ]
      [ HH.div [ css "modal-background" ] []
      , HH.div
          [ css "modal-card" ]
          [ HH.header
              [ css "modal-card-head" ]
              [ HH.p
                  [ css "modal-card-title" ]
                  [ HH.text title ]
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
    title = case operation of
      AddLiquidity -> "Add Liquidity"
      RemoveLiquidity -> "Remove Liquidity"

    renderForm = do
      let
        form = formComponent pool walletFunds operation
      HH.slot _manageLiquidityForm unit form unit HandleManageLiquidity

-- Form types
newtype ManageLiquidityForm (r :: Row Type -> Type) f
  = ManageLiquidityForm (r (ManageLiquidityRow f))

derive instance newtypeManageLiquidityForm :: Newtype (ManageLiquidityForm r f) _

type ManageLiquidityRow :: (Type -> Type -> Type -> Type) -> Row Type
type ManageLiquidityRow f
  = ( coinA :: f V.FormError (Maybe Coin) Coin
    , coinB :: f V.FormError (Maybe Coin) Coin
    , amountA :: f V.FormError (Maybe Amount) Amount
    , amountB :: f V.FormError (Maybe Amount) Amount
    , fee :: f V.FormError (Maybe Fee) Fee
    )

-- Form component types
type FormSlot
  = H.Slot (F.Query ManageLiquidityForm FormQuery FormChildSlots) LiquidityPool

type FormChildSlots
  = ( coinInputPanel :: H.Slot CoinInputPanel.Query CoinInputPanel.Message CoinSlot )

_manageLiquidityForm = Proxy :: Proxy "manageLiquidityForm"

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

prx :: F.SProxies ManageLiquidityForm
prx = F.mkSProxies (Proxy :: Proxy ManageLiquidityForm)

formComponent ::
  forall i m.
  MonadAff m =>
  Pool ->
  (Array Fund) ->
  OperationType ->
  F.Component ManageLiquidityForm FormQuery FormChildSlots i LiquidityPool m
formComponent { coinA, coinB, fee } _ operation =  -- use funds to validatie amount to avoid invest over user funds
  F.component input
    $ F.defaultSpec
        { render = render
        , handleAction = handleAction
        , handleEvent = handleEvent
        , handleQuery = handleQuery
        }
  where
  input :: i -> F.Input ManageLiquidityForm FormState m
  input _ =
    { validators:
        ManageLiquidityForm
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
                  [ HH.text title ]
              , HH.div
                  [ css "card-content" ]
                  [ HH.div
                      [ css "content" ]
                      [ coinInput CoinA coinA
                      , coinInput CoinB coinB
                      ]
                  ]
              , Field.submit "Submit"
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

    title = case operation of
      AddLiquidity -> "Deposit Amouts"
      RemoveLiquidity -> "Withdrawal Amounts"
