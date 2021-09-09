module Uniswap.Component.CoinInputPanel where

import Prelude
import Data.BigInt as BigInt
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select as Select
import Type.Proxy (Proxy(..))
import Uniswap.Component.Dropdown as Dropdown
import Uniswap.Component.HTML.Utils (css)
import Uniswap.Data.Amount (Amount(..))
import Uniswap.Data.Amount as Amount
import Uniswap.Data.Coin (Coin)

_coinInputPanel = Proxy :: Proxy "coinInputPanel"

type Slot
  = H.Slot Query Message

data Message
  = Selected Coin
  | Updated Amount
  | Cleared

data Action
  = SelectedCoin (Dropdown.Message Coin)
  | UpdatedAmount String

data Query a
  = SetAmount Amount a
  | Clear a

type Input
  = { label :: String
    , amount :: Maybe Amount
    , coin :: Maybe Coin
    , coins :: Array Coin
    }

type State
  = { coin :: Maybe Coin
    , coins :: Array Coin
    , amount :: Amount
    , label :: String
    }

type Slots
  = ( dropdown :: Dropdown.Slot Coin Unit )

coinInputPanel ::
  forall m.
  MonadAff m =>
  H.Component Query Input Message m
coinInputPanel =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              }
    }
  where
  initialState :: Input -> State
  initialState { label, coin, coins, amount } =
    { amount: fromMaybe Amount.zero amount
    , label
    , coin
    , coins
    }

  render :: State -> H.ComponentHTML Action Slots m
  render { label, coin, coins, amount } =
    HH.div
      [ css "coin-input-box" ]
      [ HH.div
          [ css "currency-select-button" ]
          [ HH.slot Dropdown._dropdown unit (Select.component Dropdown.input Dropdown.spec) coinsInput handler ]
      , HH.input
          [ css "input token-amount-input"
          , HP.value $ Amount.toString amount
          , HE.onValueInput $ UpdatedAmount
          ]
      ]
    where
    handler = SelectedCoin

    coinsInput =
      { placeholder: label
      , items: coins
      , item: coin
      }

  handleAction :: Action -> H.HalogenM State Action Slots Message m Unit
  handleAction = case _ of
    SelectedCoin message -> case message of
      Dropdown.Selected coin -> H.raise (Selected coin)
      Dropdown.Cleared -> H.raise Cleared
    UpdatedAmount str -> do
      case BigInt.fromString str of
        Nothing -> do
          oldstate <- H.get
          -- ACHTUNG: disallow to set non digit char by changing the state and then changing it back to the old state.
          H.modify_ _ { amount = Amount $ BigInt.fromInt 2 } *> H.modify_ _ { amount = oldstate.amount }
        Just bi -> do
          let
            a = Amount bi
          H.modify_ _ { amount = a }
          H.raise $ Updated a

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetAmount amount a -> do
      H.modify_ _ { amount = amount }
      pure (Just a)
    Clear a -> do
      H.modify_ \st -> st { coin = Nothing, amount = Amount.zero }
      _ <- H.queryAll Dropdown._dropdown Dropdown.clear
      H.raise Cleared
      pure (Just a)
