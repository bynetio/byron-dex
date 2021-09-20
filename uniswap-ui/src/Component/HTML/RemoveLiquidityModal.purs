module Uniswap.Component.HTML.RemoveLiquidityModal where  -- rename this component to ManageLiquidityModal

import Prelude
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Uniswap.Capability.Funds (class ManageFunds)
import Uniswap.Capability.Pool (class ManagePool)
import Uniswap.Component.CoinInputPanel as CoinInputPanel
import Uniswap.Component.HTML.Utils (css, toRight, whenElem)
import Uniswap.Data.Amount (Amount)
import Uniswap.Data.Coin (Coin)
import Uniswap.Data.Fee (Fee)
import Uniswap.Data.LiquidityPool (LiquidityPool)
import Uniswap.Form.Field as Field
import Uniswap.Form.Validation as V
import Web.Event.Event as Event

-- Modal comonent to add liquidity
data Action
  = HandleRemoveLiquidity LiquidityPool
  | CloseModal

data Query a
  = ShowModal Pool a

type Pool
  = { coinA :: Coin
    , coinB :: Coin
    , fee :: Fee
    }

type State
  = { pool :: Maybe Pool
    }

type ChildSlots
  = ( removeLiquidityForm :: FormSlot Unit )

modalProxy = Proxy :: Proxy "removeLiquidityModal"

component ::
  forall o m.
  MonadAff m =>
  ManagePool m =>
  ManageFunds m =>
  H.Component Query Unit o m
component =
  H.mkComponent
    { initialState: \_ -> { pool: Nothing }
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              }
    }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    HandleRemoveLiquidity form -> do
      -- removeFromLiquidityPool form
      H.modify_ _ { pool = Nothing }
    CloseModal -> H.modify_ _ { pool = Nothing }

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    ShowModal pool a -> do
      H.modify_ _ { pool = Just pool }
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = do
    either identity identity renderEither
    where
    renderEither = do
      pool <- toRight state.pool
      pure $ renderModal pool

  renderModal pool =
    HH.div
      [ css "modal is-active" ]
      [ HH.div [ css "modal-background" ] []
      , HH.div
          [ css "modal-card" ]
          [ HH.header
              [ css "modal-card-head" ]
              [ HH.p
                  [ css "modal-card-title" ]
                  [ HH.text "Remove Liquidity" ]
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
        form = formComponent pool
      HH.slot _removeLiquidityForm unit form unit HandleRemoveLiquidity

-- Form types
newtype RemoveLiquidityForm (r :: Row Type -> Type) f
  = RemoveLiquidityForm (r (RemoveLiquidityRow f))

derive instance newtypeRemoveLiquidityForm :: Newtype (RemoveLiquidityForm r f) _

type RemoveLiquidityRow :: (Type -> Type -> Type -> Type) -> Row Type
type RemoveLiquidityRow f
  = ( coinA :: f V.FormError (Maybe Coin) Coin
    , coinB :: f V.FormError (Maybe Coin) Coin
    , amountA :: f V.FormError (Maybe Amount) Amount
    , amountB :: f V.FormError (Maybe Amount) Amount
    , fee :: f V.FormError (Maybe Fee) Fee
    --, diff :: f V.FormError (Maybe Int) Int
    )

-- Form component types
type FormSlot
  = H.Slot (F.Query RemoveLiquidityForm FormQuery FormChildSlots) LiquidityPool

type FormChildSlots
  = ( coinInputPanel :: H.Slot CoinInputPanel.Query CoinInputPanel.Message CoinSlot )

_removeLiquidityForm = Proxy :: Proxy "removeLiquidityForm"

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

prx :: F.SProxies RemoveLiquidityForm
prx = F.mkSProxies (Proxy :: Proxy RemoveLiquidityForm)

formComponent ::
  forall i m.
  MonadAff m =>
  Pool ->
  F.Component RemoveLiquidityForm FormQuery FormChildSlots i LiquidityPool m
formComponent { coinA, coinB, fee } =  -- use funds to validatie amount to avoid invest over user funds
  F.component input
    $ F.defaultSpec
        { render = render
        , handleAction = handleAction
        , handleEvent = handleEvent
        , handleQuery = handleQuery
        }
  where
  input :: i -> F.Input RemoveLiquidityForm FormState m
  input _ =
    { validators:
        RemoveLiquidityForm
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
                  [ HH.text "Withdrawal Liquidity" ]
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
