module Uniswap.Page.ConnectWallet where

import Prelude
import Uniswap.Data.Wallet (ConnectWalletFields)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, updateStore)
import Type.Proxy (Proxy(..))
import Uniswap.Capability.Navigate (class Navigate, navigate)
import Uniswap.Component.HTML.Header (header)
import Uniswap.Component.HTML.Utils (css, whenElem)
import Uniswap.Data.Route (Route(..))
import Uniswap.Form.Field as Field
import Uniswap.Form.Validation as V
import Uniswap.Store as Store
import Web.Event.Event as Event

data Action
  = HandleConnectFrom ConnectWalletFields

type State
  = { redirect :: Boolean }

type Input
  = { redirect :: Boolean }

type ChildSlots
  = ( formless :: F.Slot ConnectWalletForm FormQuery () ConnectWalletFields Unit )

component ::
  forall q o m.
  MonadAff m =>
  MonadStore Store.Action Store.Store m =>
  Navigate m =>
  H.Component q Input o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    HandleConnectFrom fields -> do
      updateStore $ Store.ConnectWallet fields
      void $ H.query F._formless unit $ F.injQuery $ SetConnectError false unit
      st <- H.get
      when st.redirect (navigate Home)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render _ =
    container
      [ HH.h1
          [ css "text-xs-center" ]
          [ HH.text "Connect a Wallet" ]
      , HH.slot F._formless unit formComponent unit HandleConnectFrom
      ]
    where
    container html =
      HH.div
        [ css "auth-page" ]
        [ header Nothing ConnectWallet
        , HH.div
            [ css "container page" ]
            [ HH.div
                [ css "row" ]
                [ HH.div
                    [ css "col-md-6 offset-md-3 col-xs12" ]
                    html
                ]
            ]
        ]

newtype ConnectWalletForm (r :: Row Type -> Type) f
  = ConnectWalletForm
  ( r
      ( instance :: f V.FormError String String )
  )

derive instance newtypeConnectWalletForm :: Newtype (ConnectWalletForm r f) _

data FormQuery a
  = SetConnectError Boolean a

derive instance functorFormQuery :: Functor FormQuery

data FormAction
  = Submit Event.Event

formComponent ::
  forall i slots m.
  MonadAff m =>
  F.Component ConnectWalletForm FormQuery slots i ConnectWalletFields m
formComponent =
  F.component formInput
    $ F.defaultSpec
        { render = renderConnect
        , handleEvent = handleEvent
        , handleQuery = handleQuery
        , handleAction = handleAction
        }
  where
  formInput :: i -> F.Input ConnectWalletForm ( connectError :: Boolean ) m
  formInput _ =
    { validators:
        ConnectWalletForm
          { instance: V.required >>> V.minLength 35 >>> V.maxLength 36 }
    , initialInputs: Nothing
    , connectError: false
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
    SetConnectError bool a -> do
      H.modify_ _ { connectError = bool }
      pure (Just a)

  proxies = F.mkSProxies (Proxy :: _ ConnectWalletForm)

  renderConnect { form, connectError } =
    HH.form
      [ HE.onSubmit \ev -> F.injAction $ Submit ev ]
      [ whenElem connectError \_ ->
          HH.div
            [ css "error-messages" ]
            [ HH.text "Instance ID is invalid" ]
      , HH.fieldset_
          [ Field.input proxies.instance form
              [ HP.placeholder "Instance ID"
              , HP.type_ HP.InputText
              ]
          , Field.submit "Connect"
          ]
      ]
