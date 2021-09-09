module Uniswap.Store where

import Data.Maybe (Maybe(..))
import Uniswap.Api.Request (ApiURL)
import Uniswap.Data.SlippageTolerance (SlippageTolerance)
import Uniswap.Data.Wallet (Wallet)

type Store
  = { currentWallet :: Maybe Wallet
    , apiUrl :: ApiURL
    , slippageTolerance :: SlippageTolerance
    }

data Action
  = ConnectWallet Wallet
  | DisconnectWallet
  | SetSlippageTolerance SlippageTolerance

reduce :: Store -> Action -> Store
reduce store = case _ of
  ConnectWallet wallet -> store { currentWallet = Just wallet }
  DisconnectWallet -> store { currentWallet = Nothing }
  SetSlippageTolerance tolerance -> store { slippageTolerance = tolerance }
