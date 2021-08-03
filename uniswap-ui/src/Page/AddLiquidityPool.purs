module Uniswap.Page.AddLiquidityPool where

import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Type.Proxy (Proxy(..))
import Uniswap.Capability.Navigate (class Navigate, navigate)
import Uniswap.Capability.Pool (class ManagePool, createLiquidityPool)
import Uniswap.Component.HTML.Header (header)
import Uniswap.Component.HTML.Utils (css, whenElem)
import Uniswap.Data.Amount (Amount)
import Uniswap.Data.Coin (CurrencySymbol, TokenName)
import Uniswap.Data.Fee (Fee)
import Uniswap.Data.LiquidityPool (LiquidityPool)
import Uniswap.Data.Route (Route(..))
import Uniswap.Data.Wallet (Wallet)
import Uniswap.Form.Field as Field
import Uniswap.Form.Validation as V
import Uniswap.Store as Store
import Web.Event.Event as Event

data Action
  = HandleCreateForm LiquidityPoolFields

type State
  = { currentWallet :: Maybe Wallet }

type ChildSlots
  = ( formless :: F.Slot AddPoolForm FormQuery () LiquidityPoolFields Unit )

type LiquidityPoolFields
  = { tokenNameA :: TokenName
    , currencySymbolA :: CurrencySymbol
    , amountA :: Amount
    , tokenNameB :: TokenName
    , currencySymbolB :: CurrencySymbol
    , amountB :: Amount
    , fee :: Fee
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
        { initialState:
            \{ context: currentWallet } ->
              { currentWallet }
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    HandleCreateForm { tokenNameA, currencySymbolA, amountA, tokenNameB, currencySymbolB, amountB, fee } -> do
      let
        lp :: LiquidityPool
        lp =
          { coinA: { tokenName: tokenNameA, currencySymbol: currencySymbolA }
          , amountA: amountA
          , coinB: { tokenName: tokenNameB, currencySymbol: currencySymbolB }
          , amountB: amountB
          , fee: fee
          }
      createLiquidityPool lp
        *> do
            void $ H.query F._formless unit $ F.injQuery $ SetFormError false unit
            navigate Pools

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { currentWallet } =
    container
      [ HH.h1
          [ css "text-xs-center" ]
          [ HH.text "Create a Pool" ]
      , HH.slot F._formless unit formComponent unit HandleCreateForm
      ]
    where
    container html =
      HH.div_
        [ header currentWallet AddPool
        , HH.div
            [ css "" ]
            [ HH.div
                [ css "container page" ]
                [ HH.div
                    [ css "row" ]
                    [ HH.div
                        [ css "col-md-6 offset-md-3 col-xs12" ]
                        html
                    ]
                ]
            ]
        ]

newtype AddPoolForm (r :: Row Type -> Type) f
  = AddPoolForm
  ( r
      ( tokenNameA :: f V.FormError String TokenName
      , currencySymbolA :: f V.FormError String CurrencySymbol
      , tokenNameB :: f V.FormError String TokenName
      , currencySymbolB :: f V.FormError String CurrencySymbol
      , amountA :: f V.FormError String Amount
      , amountB :: f V.FormError String Amount
      , fee :: f V.FormError String Fee
      )
  )

derive instance newtypeAddPoolFields :: Newtype (AddPoolForm r f) _

data FormAction
  = Submit Event.Event

data FormQuery a
  = SetFormError Boolean a

derive instance functorFormQuery :: Functor FormQuery

formComponent ::
  forall i slots m.
  MonadAff m =>
  F.Component AddPoolForm FormQuery slots i LiquidityPoolFields m
formComponent =
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
          { tokenNameA: V.required >>> V.tokenNameFormat
          , currencySymbolA: V.required >>> V.currencySymbolFormat
          , amountA: V.required >>> V.amountFormat
          , tokenNameB: V.required >>> V.tokenNameFormat
          , currencySymbolB: V.required >>> V.currencySymbolFormat
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
    where
    eval act = F.handleAction handleAction handleEvent act

  handleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetFormError bool a -> do
      H.modify_ _ { formError = bool }
      pure (Just a)

  proxies = F.mkSProxies (Proxy :: _ AddPoolForm)

  renderCreateLP { form, formError } =
    HH.form
      [ HE.onSubmit \ev -> F.injAction $ Submit ev ]
      [ whenElem formError \_ ->
          HH.div
            [ css "error-messages" ]
            [ HH.text "all fields are required" ]
      , HH.fieldset_
          [ Field.input proxies.tokenNameA form
              [ HP.placeholder "Token Name A"
              , HP.type_ HP.InputText
              ]
          , Field.input proxies.currencySymbolA form
              [ HP.placeholder "Currency Symbol A"
              , HP.type_ HP.InputText
              ]
          , Field.input proxies.amountA form
              [ HP.placeholder "Amount A"
              , HP.type_ HP.InputNumber
              ]
          , Field.input proxies.tokenNameB form
              [ HP.placeholder "Token Name B"
              , HP.type_ HP.InputText
              ]
          , Field.input proxies.currencySymbolB form
              [ HP.placeholder "Currency Symbol B"
              , HP.type_ HP.InputText
              ]
          , Field.input proxies.amountB form
              [ HP.placeholder "Amount B"
              , HP.type_ HP.InputNumber
              ]
          , Field.input proxies.fee form
              [ HP.placeholder "Fee"
              , HP.type_ HP.InputText
              ]
          , Field.submit "Create!"
          ]
      ]
