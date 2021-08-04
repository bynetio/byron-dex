module Uniswap.Store where

import Data.Maybe (Maybe(..))
import Uniswap.Api.Request (ApiURL)
import Uniswap.Data.Wallet (Wallet)

type Store
  = { currentWallet :: Maybe Wallet
    , apiUrl :: ApiURL
    }

data Action
  = ConnectWallet Wallet
  | DisconnectWallet

reduce :: Store -> Action -> Store
reduce store = case _ of
  ConnectWallet wallet -> store { currentWallet = Just wallet }
  DisconnectWallet -> store { currentWallet = Nothing }
