module Uniswap.Component.HTML.SwapForm where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (logShow)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Type.Proxy (Proxy(..))
import Uniswap.Capability.Swap (class ManageSwap, indirectSwapPreview)
import Uniswap.Component.CoinInputPanel as CoinInputPanel
import Uniswap.Component.HTML.Utils (css)
import Uniswap.Data.Amount (Amount)
import Uniswap.Data.Amount as Amount
import Uniswap.Data.Coin (Coin)
import Uniswap.Data.Funds (Fund)
import Uniswap.Data.Swap (SwapPreviewResponse)
import Uniswap.Form.Field as Field
import Uniswap.Form.Validation as V
import Web.Event.Event as Event

-- types
_swapForm = Proxy :: Proxy "swapForm"

newtype SwapForm (r :: Row Type -> Type) f
  = SwapForm (r (SwapRow f))

derive instance newtypeSwapForm :: Newtype (SwapForm r f) _

type SwapRow :: (Type -> Type -> Type -> Type) -> Row Type
type SwapRow f
  = ( coinA :: f V.FormError (Maybe Coin) Coin
    , coinB :: f V.FormError (Maybe Coin) Coin
    , amountA :: f V.FormError (Maybe Amount) Amount
    , amountB :: f V.FormError (Maybe Amount) Amount
    )

-- conponent types
type Slot
  = H.Slot (F.Query SwapForm Query Slots) SwapPreviewResponse -- Rename this record...

data Query a
  = SetFormError Boolean a
  | Clear a

derive instance functorQuery :: Functor Query

type State
  = ( formError :: Boolean, previewed :: Boolean )

data Action
  = Submit Event.Event
  | HandleCoinInputPanel CoinSlot CoinInputPanel.Message
  | Reset

type Slots
  = ( coinInputPanel :: CoinInputPanel.Slot CoinSlot )

data CoinSlot
  = CoinA
  | CoinB

derive instance eqCoinSlot :: Eq CoinSlot

derive instance ordCoinSlot :: Ord CoinSlot

prx :: F.SProxies SwapForm
prx = F.mkSProxies (Proxy :: Proxy SwapForm)

component ::
  forall i m.
  MonadAff m =>
  ManageSwap m =>
  (Array Fund) ->
  F.Component SwapForm Query Slots i SwapPreviewResponse m
component funds =
  F.component input
    $ F.defaultSpec
        { render = render
        , handleAction = handleAction
        , handleEvent = handleEvent
        , handleQuery = handleQuery
        }
  where
  input :: i -> F.Input SwapForm State m
  input _ =
    { validators:
        SwapForm
          { coinA: V.exists
          , coinB: V.exists
          , amountA: V.exists
          , amountB: V.exists
          }
    , initialInputs: Nothing
    , formError: false
    , previewed: false
    }

  eval act = F.handleAction handleAction handleEvent act

  handleEvent = case _ of
    F.Submitted out -> do
      H.raise (F.unwrapOutputFields out)
      void $ H.fork $ handleAction $ Reset
    F.Changed form -> do
      let
        mbCoinA = F.getOutput prx.coinA form.form

        mbCoinB = F.getOutput prx.coinB form.form

        mbAmount = F.getOutput prx.amountA form.form

        mbBody = do
          coinA <- mbCoinA
          coinB <- mbCoinB
          amount <- mbAmount
          pure $ { coinA, coinB, amount }
      { previewed } <- H.get
      case mbBody of
        Just body
          | not previewed -> do
            resp <- indirectSwapPreview body
            case resp of
              Just b -> do
                H.tell CoinInputPanel._coinInputPanel CoinB (CoinInputPanel.SetAmount b.amountB)
                H.modify_ _ { previewed = true }
                eval $ F.set prx.amountB (Just b.amountB)
              Nothing -> pure unit
        _ -> pure unit

  handleQuery ::
    forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetFormError bool a -> do
      H.modify_ _ { formError = bool }
      pure (Just a)
    Clear a -> do
      void $ H.fork $ handleAction $ Reset
      pure (Just a)

  handleAction = case _ of
    Submit _ -> do
      eval F.submit
    HandleCoinInputPanel slot message -> case slot of
      CoinA -> case message of
        CoinInputPanel.Selected coin -> do
          H.modify_ _ { previewed = false }
          eval $ F.setValidate prx.coinA (Just coin)
        CoinInputPanel.Updated amount -> do
          H.modify_ _ { previewed = false }
          eval $ F.setValidate prx.amountA (Just amount)
          liftAff $ logShow (Amount.toString amount)
        CoinInputPanel.Cleared -> do
          H.modify_ _ { previewed = false }
          eval $ F.setValidate prx.coinA Nothing
          eval $ F.setValidate prx.amountA Nothing
      CoinB -> case message of
        CoinInputPanel.Selected coin -> do
          H.modify_ _ { previewed = false }
          eval $ F.setValidate prx.coinB (Just coin)
        CoinInputPanel.Updated _ -> pure unit
        CoinInputPanel.Cleared -> do
          H.modify_ _ { previewed = false }
          eval $ F.setValidate prx.coinB Nothing
    Reset -> do
      H.modify_ \st -> st { formError = false, previewed = false }
      void $ H.tell CoinInputPanel._coinInputPanel CoinA CoinInputPanel.Clear
      void $ H.tell CoinInputPanel._coinInputPanel CoinB CoinInputPanel.Clear

  render _ =
    HH.form
      [ css "content"
      , HE.onSubmit \ev -> F.injAction $ Submit ev
      ]
      [ renderInputCoin CoinA "Select a Coin A"
      , renderArrow
      , renderInputCoin CoinB "Select a Coin B"
      , HH.input
          [ css "swap-button is-link is-light "
          , HP.type_ HP.InputSubmit
          , HP.value "Swap"
          ]
      ]
    where
    renderInputCoin slot label = HH.slot CoinInputPanel._coinInputPanel slot CoinInputPanel.coinInputPanel inputForm handler
      where
      inputForm =
        { coin: Nothing
        , amount: Nothing
        , label
        , coins:
            (map (\f -> f.coin) funds)
        }

      handler = F.injAction <<< HandleCoinInputPanel slot

    -- move it to utils
    renderArrow =
      HH.div
        [ css "swap-arrow" ]
        [ SE.svg
            [ SA.viewBox 0.0 0.0 24.0 24.0
            , SA.width 16.0
            , SA.height 16.0
            , SA.stroke $ Just $ SA.RGB 143 150 173
            ]
            [ SE.line [ SA.x1 12.0, SA.y1 5.0, SA.x2 12.0, SA.y2 19.0, SA.strokeWidth 2.0 ]
            , SE.line [ SA.x1 5.0, SA.y1 12.0, SA.x2 12.0, SA.y2 19.0, SA.strokeWidth 2.0 ]
            , SE.line [ SA.x1 12.0, SA.y1 19.0, SA.x2 19.0, SA.y2 12.0, SA.strokeWidth 2.0 ]
            ]
        ]
