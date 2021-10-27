{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

module Main where

import           Data.Default
import           Data.Functor           (void)
import qualified Data.Map               as Map
import           Dex.OffChain
import           Dex.Trace              (customTraceConfig)
import           Dex.Types
import           Plutus.Trace.Emulator  as Emulator
import qualified Plutus.V1.Ledger.Ada   as Ada
import qualified Plutus.V1.Ledger.Value as Value
import           Wallet.Emulator.Wallet as Wallet

customSymbol :: [Char]
customSymbol = "ff"

customToken :: [Char]
customToken = "PLN"

customSymbolsAndTokens :: [(Value.CurrencySymbol, Value.TokenName)]
customSymbolsAndTokens = [("ff", "coin1"), ("ee", "coin2"), ("dd", "coin3"), ("cc", "coin4"), ("bb", "coin5")]

customSymbol2 :: [Char]
customSymbol2 = "ee"

customToken2 :: [Char]
customToken2 = "BTC"

customSymbol3 :: [Char]
customSymbol3 = "dd"

customToken3 :: [Char]
customToken3 = "ETH"

emulatorCfg :: EmulatorConfig
emulatorCfg = EmulatorConfig (Left $ Map.fromList ([(knownWallet i, v) | i <- [1 .. 4]])) def def
  where
    v = Ada.lovelaceValueOf 100_000_000 <> mconcat (map (\(symbol,tokenName) -> Value.singleton symbol tokenName 100_000_000) customSymbolsAndTokens)

data UseCase = UseCase { description :: String
                       , trace       :: EmulatorTrace ()
                       }

simpleSwapCase :: UseCase
simpleSwapCase = UseCase "Swap offers: W1 200ff -> 600ee, W2 650ee -> 200ff" $ do
  h1 <- activateContractWallet (knownWallet 1) dexEndpoints
  h2 <- activateContractWallet (knownWallet 2) dexEndpoints

  void $ callEndpoint @"createSellOrder" h1 (Request "a" 1 (SellOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 600))
  void $ waitNSlots 2
  void $ callEndpoint @"createSellOrder" h2 (Request "b" 2 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 650 200))
  void $ waitNSlots 2

  void $ callEndpoint @"perform" h1 (Request "c" 3 ())
  void $ waitNSlots 2

  void $ callEndpoint @"collectFunds" h1 (Request "d" 4 ())
  void $ waitNSlots 2
  void $ callEndpoint @"collectFunds" h2 (Request "e" 5 ())
  void $ waitNSlots 2

simpleLiquidityOrderSwapCase :: UseCase
simpleLiquidityOrderSwapCase = UseCase "Liquidity Orders: W1 200ff -> 400ee (1,100), Swap offers: W2 650ee -> 200ff" $ do
  h1 <- activateContractWallet (knownWallet 1) dexEndpoints
  h2 <- activateContractWallet (knownWallet 2) dexEndpoints

  void $ callEndpoint @"createLiquidityOrder" h1 (Request "b" 1 (LiquidityOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 400 (1,100)))
  void $ waitNSlots 2
  void $ callEndpoint @"createSellOrder" h2 (Request "b" 2 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 650 200))
  void $ waitNSlots 2

  void $ callEndpoint @"perform" h1 (Request "c" 3 ())
  void $ waitNSlots 2

  void $ callEndpoint @"collectFunds" h2 (Request "e" 5 ())
  void $ waitNSlots 2

useCases :: [UseCase]
useCases = [ simpleSwapCase
           , simpleLiquidityOrderSwapCase
           ]

runUseCase :: UseCase -> IO ()
runUseCase useCase = do
  putStrLn (description useCase)
  runEmulatorTraceIO' customTraceConfig emulatorCfg $ trace useCase

main :: IO ()
main = mapM_ runUseCase useCases
