module Uniswap.Store where

import Prelude

import Uniswap.Data.Wallet (Wallet)
import Data.Maybe (Maybe(..))


type Store =
  { currentWallet :: Maybe Wallet
  }

data Action
  = ConnectWallet Wallet
  | DisconnectWallet

reduce :: Store -> Action -> Store
reduce store = case _ of
  ConnectWallet wallet ->
    store { currentWallet = Just wallet }

  DisconnectWallet ->
    store { currentWallet = Nothing }
