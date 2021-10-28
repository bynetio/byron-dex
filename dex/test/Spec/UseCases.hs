{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# OPTIONS_GHC -Wno-orphans    #-}

module Spec.UseCases (main, tests) where

import           Data.Default
import           Data.Functor           (void)
import qualified Data.Map               as Map
import           Plutus.Trace.Emulator  as Emulator
import qualified Plutus.V1.Ledger.Ada   as Ada
import qualified Plutus.V1.Ledger.Value as Value
import           Wallet.Emulator.Wallet as Wallet
import           Test.Tasty
import           Test.Tasty.HUnit

import           Dex.OffChain
import           Dex.Trace              (customTraceConfig)
import           Dex.Types

customSymbolsAndTokens :: [(Value.CurrencySymbol, Value.TokenName)]
customSymbolsAndTokens = [("ff", "coin1"), ("ee", "coin2"), ("dd", "coin3"), ("cc", "coin4"), ("bb", "coin5")]

emulatorCfg :: EmulatorConfig
emulatorCfg = EmulatorConfig (Left $ Map.fromList ([(knownWallet i, v) | i <- [1 .. 4]])) def def
  where
    v = Ada.lovelaceValueOf 100_000_000 <> mconcat (map (\(symbol,tokenName) -> Value.singleton symbol tokenName 100_000_000) customSymbolsAndTokens)

performTest :: EmulatorTrace () -> IO ()
performTest = runEmulatorTraceIO' customTraceConfig emulatorCfg

-- wallet W1 wants to exchange 200ff for 600ee
-- wallet W2 wants to exchange 600ee for 200ff
-- W1 meets fitting offer from W2
-- W3 is a performer
-- W1 pays 200ff and gets 600ee
-- W2 pays 600ee and gets 200ff
simpleSwap :: TestTree
simpleSwap = testCase "Swap offers: W1 200ff -> 600ee, W2 600ee -> 200ff" $ do
  performTest $ do
    h1 <- activateContractWallet (knownWallet 1) dexEndpoints
    h2 <- activateContractWallet (knownWallet 2) dexEndpoints
    h3 <- activateContractWallet (knownWallet 3) dexEndpoints

    void $ callEndpoint @"createSellOrder" h1 (Request "a" 1 (SellOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 600))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h2 (Request "b" 2 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 600 200))
    void $ waitNSlots 2

    void $ callEndpoint @"perform" h3 (Request "c" 3 ())
    void $ waitNSlots 2

    void $ callEndpoint @"collectFunds" h1 (Request "d" 4 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h2 (Request "e" 5 ())
    void $ waitNSlots 2

-- wallet W1 wants to exchange 200ff for 600ee
-- wallet W2 wants to exchange 400ee for 200ff
-- W1 meets fitting offer from W2
-- W3 is a performer
-- W1 pays 200ff and gets 600ee
-- W2 pays 400ee and gets 200ff
-- W3 pays 200ee
swapUnderExpectations :: TestTree
swapUnderExpectations = testCase "Swap offers: W1 200ff -> 600ee, W2 400ee -> 200ff" $ do
  performTest $ do
    h1 <- activateContractWallet (knownWallet 1) dexEndpoints
    h2 <- activateContractWallet (knownWallet 2) dexEndpoints
    h3 <- activateContractWallet (knownWallet 3) dexEndpoints

    void $ callEndpoint @"createSellOrder" h1 (Request "a" 1 (SellOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 600))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h2 (Request "b" 2 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 400 200))
    void $ waitNSlots 2

    void $ callEndpoint @"perform" h3 (Request "c" 3 ())
    void $ waitNSlots 2

    void $ callEndpoint @"collectFunds" h1 (Request "d" 4 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h2 (Request "e" 5 ())
    void $ waitNSlots 2

-- wallet W1 wants to exchange 200ff for 600ee
-- wallet W2 wants to exchange 400ee for 200ff
-- W1 meets fitting offer from W2
-- W1 is a performer
-- W1 pays 200ff and gets 400ee
-- W2 pays 400ee and gets 200ff
swapUnderExpectationsW1Performs :: TestTree
swapUnderExpectationsW1Performs = testCase "Swap offers: W1 200ff -> 600ee, W2 400ee -> 200ff" $ do
  performTest $ do
    h1 <- activateContractWallet (knownWallet 1) dexEndpoints
    h2 <- activateContractWallet (knownWallet 2) dexEndpoints

    void $ callEndpoint @"createSellOrder" h1 (Request "a" 1 (SellOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 600))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h2 (Request "b" 2 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 400 200))
    void $ waitNSlots 2

    void $ callEndpoint @"perform" h1 (Request "c" 3 ())
    void $ waitNSlots 2

    void $ callEndpoint @"collectFunds" h1 (Request "d" 4 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h2 (Request "e" 5 ())
    void $ waitNSlots 2

-- wallet W1 wants to exchange 200ff for 600ee
-- wallet W2 wants to exchange 400ee for 200ff
-- W1 meets fitting offer from W2
-- W2 is a performer
-- W1 pays 200ff and gets 600ee
-- W2 pays 600ee and gets 200ff
swapUnderExpectationsW2Performs :: TestTree
swapUnderExpectationsW2Performs = testCase "Swap offers: W1 200ff -> 600ee, W2 400ee -> 200ff" $ do
  performTest $ do
    h1 <- activateContractWallet (knownWallet 1) dexEndpoints
    h2 <- activateContractWallet (knownWallet 2) dexEndpoints

    void $ callEndpoint @"createSellOrder" h1 (Request "a" 1 (SellOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 600))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h2 (Request "b" 2 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 400 200))
    void $ waitNSlots 2

    void $ callEndpoint @"perform" h2 (Request "c" 3 ())
    void $ waitNSlots 2

    void $ callEndpoint @"collectFunds" h1 (Request "d" 4 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h2 (Request "e" 5 ())
    void $ waitNSlots 2

-- wallet W1 wants to exchange 200ff for 400ee
-- wallet W2 wants to exchange 600ee for 200ff
-- W1 meets fitting offer from W2
-- W3 is a performer
-- W1 pays 200ff and gets 400ee
-- W2 pays 600ee and gets 200ff
-- W3 gets 200ee
swapAboveExpectations :: TestTree
swapAboveExpectations = testCase "Swap offers: W1 200ff -> 400ee, W2 600ee -> 200ff" $ do
  performTest $ do
    h1 <- activateContractWallet (knownWallet 1) dexEndpoints
    h2 <- activateContractWallet (knownWallet 2) dexEndpoints
    h3 <- activateContractWallet (knownWallet 3) dexEndpoints

    void $ callEndpoint @"createSellOrder" h1 (Request "a" 1 (SellOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 400))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h2 (Request "b" 2 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 600 200))
    void $ waitNSlots 2

    void $ callEndpoint @"perform" h3 (Request "c" 3 ())
    void $ waitNSlots 2

    void $ callEndpoint @"collectFunds" h1 (Request "d" 4 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h2 (Request "e" 5 ())
    void $ waitNSlots 2

-- wallet W1 wants to exchange 200ff for 400ee
-- wallet W2 wants to exchange 600ee for 200ff
-- W1 meets fitting offer from W2
-- W1 is a performer
-- W1 pays 200ff and gets 600ee
-- W2 pays 600ee and gets 200ff
swapAboveExpectationsW1Performs :: TestTree
swapAboveExpectationsW1Performs = testCase "Swap offers: W1 200ff -> 400ee, W2 600ee -> 200ff" $ do
  performTest $ do
    h1 <- activateContractWallet (knownWallet 1) dexEndpoints
    h2 <- activateContractWallet (knownWallet 2) dexEndpoints

    void $ callEndpoint @"createSellOrder" h1 (Request "a" 1 (SellOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 400))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h2 (Request "b" 2 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 600 200))
    void $ waitNSlots 2

    void $ callEndpoint @"perform" h1 (Request "c" 3 ())
    void $ waitNSlots 2

    void $ callEndpoint @"collectFunds" h1 (Request "d" 4 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h2 (Request "e" 5 ())
    void $ waitNSlots 2

-- wallet W1 wants to exchange 200ff for 400ee
-- wallet W2 wants to exchange 600ee for 200ff
-- W1 meets fitting offer from W2
-- W2 is a performer
-- W1 pays 200ff
-- W2 gets 200ff
swapAboveExpectationsW2Performs :: TestTree
swapAboveExpectationsW2Performs = testCase "Swap offers: W1 200ff -> 400ee, W2 600ee -> 200ff" $ do
  performTest $ do
    h1 <- activateContractWallet (knownWallet 1) dexEndpoints
    h2 <- activateContractWallet (knownWallet 2) dexEndpoints

    void $ callEndpoint @"createSellOrder" h2 (Request "a" 1 (SellOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 400))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h2 (Request "b" 2 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 600 200))
    void $ waitNSlots 2

    void $ callEndpoint @"perform" h1 (Request "c" 3 ())
    void $ waitNSlots 2

    void $ callEndpoint @"collectFunds" h1 (Request "d" 4 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h2 (Request "e" 5 ())
    void $ waitNSlots 2

-- wallet W1 wants to exchange 200ff for 600ee
-- wallet W2 wants to exchange 600ee for 200ff
-- wallet W3 wants to exchange 600ee for 200ff
-- W1 meets fitting offer from W2
-- W1 meets fitting offer from W3
-- W4 is a performer
-- W1 pays 200ff and gets 600ee
-- W2 pays 600ee and gets 200ff
-- W3 pays 600ee and gets 200ff
-- W4 pays 200ff and gets 600ee
swapRace :: TestTree
swapRace = testCase "Swap offers: W1 200ff -> 600ee, W2 600ee -> 200ff, W3 600ee -> 200ff" $ do
  performTest $ do
    h1 <- activateContractWallet (knownWallet 1) dexEndpoints
    h2 <- activateContractWallet (knownWallet 2) dexEndpoints
    h3 <- activateContractWallet (knownWallet 3) dexEndpoints
    h4 <- activateContractWallet (knownWallet 4) dexEndpoints

    void $ callEndpoint @"createSellOrder" h1 (Request "a" 1 (SellOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 600))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h2 (Request "b" 2 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 600 200))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h3 (Request "c" 3 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 600 200))
    void $ waitNSlots 2

    void $ callEndpoint @"perform" h4 (Request "d" 4 ())
    void $ waitNSlots 2

    void $ callEndpoint @"collectFunds" h1 (Request "e" 5 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h2 (Request "f" 6 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h3 (Request "g" 7 ())
    void $ waitNSlots 2

-- wallet W1 wants to exchange 200ff for 600ee
-- wallet W2 wants to exchange 600ee for 200ff
-- wallet W3 wants to exchange 600ee for 200ff
-- W1 meets fitting offer from W2
-- W1 meets fitting offer from W3
-- W1 is a performer
-- W1 pays 400ff and gets 1200ee
-- W2 pays 600ee and gets 200ff
-- W3 pays 600ee and gets 200ff
swapRaceW1Performs :: TestTree
swapRaceW1Performs = testCase "Swap offers: W1 200ff -> 600ee, W2 600ee -> 200ff, W3 600ee -> 200ff" $ do
  performTest $ do
    h1 <- activateContractWallet (knownWallet 1) dexEndpoints
    h2 <- activateContractWallet (knownWallet 2) dexEndpoints
    h3 <- activateContractWallet (knownWallet 3) dexEndpoints

    void $ callEndpoint @"createSellOrder" h1 (Request "a" 1 (SellOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 600))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h2 (Request "b" 2 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 600 200))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h3 (Request "c" 3 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 600 200))
    void $ waitNSlots 2

    void $ callEndpoint @"perform" h1 (Request "d" 4 ())
    void $ waitNSlots 2

    void $ callEndpoint @"collectFunds" h1 (Request "e" 5 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h2 (Request "f" 6 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h3 (Request "g" 7 ())
    void $ waitNSlots 2

-- wallet W1 wants to exchange 200ff for 600ee
-- wallet W2 wants to exchange 600ee for 200ff
-- wallet W3 wants to exchange 600ee for 200ff
-- W1 meets fitting offer from W2
-- W1 meets fitting offer from W3
-- W2 is a performer
-- W1 pays 200ff and gets 600ee
-- W3 pays 600ee and gets 200ff
swapRaceW2Performs :: TestTree
swapRaceW2Performs = testCase "Swap offers: W1 200ff -> 600ee, W2 600ee -> 200ff, W3 600ee -> 200ff" $ do
  performTest $ do
    h1 <- activateContractWallet (knownWallet 1) dexEndpoints
    h2 <- activateContractWallet (knownWallet 2) dexEndpoints
    h3 <- activateContractWallet (knownWallet 3) dexEndpoints

    void $ callEndpoint @"createSellOrder" h1 (Request "a" 1 (SellOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 600))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h2 (Request "b" 2 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 600 200))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h3 (Request "c" 3 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 600 200))
    void $ waitNSlots 2

    void $ callEndpoint @"perform" h2 (Request "d" 4 ())
    void $ waitNSlots 2

    void $ callEndpoint @"collectFunds" h1 (Request "e" 5 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h2 (Request "f" 6 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h3 (Request "g" 7 ())
    void $ waitNSlots 2

-- wallet W1 wants to exchange 200ff for 600ee
-- wallet W2 wants to exchange 600ee for 200ff
-- wallet W3 wants to exchange 600ee for 200ff
-- W1 meets fitting offer from W2
-- W1 meets fitting offer from W3
-- W3 is a performer
-- W1 pays 200ff and gets 600ee
-- W2 pays 600ee and gets 200ff
swapRaceW3Performs :: TestTree
swapRaceW3Performs = testCase "Swap offers: W1 200ff -> 600ee, W2 600ee -> 200ff, W3 600ee -> 200ff" $ do
  performTest $ do
    h1 <- activateContractWallet (knownWallet 1) dexEndpoints
    h2 <- activateContractWallet (knownWallet 2) dexEndpoints
    h3 <- activateContractWallet (knownWallet 3) dexEndpoints

    void $ callEndpoint @"createSellOrder" h1 (Request "a" 1 (SellOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 600))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h2 (Request "b" 2 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 600 200))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h3 (Request "c" 3 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 600 200))
    void $ waitNSlots 2

    void $ callEndpoint @"perform" h3 (Request "d" 4 ())
    void $ waitNSlots 2

    void $ callEndpoint @"collectFunds" h1 (Request "e" 5 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h2 (Request "f" 6 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h3 (Request "g" 7 ())
    void $ waitNSlots 2

-- wallet W1 sets liquidity of 200ff and 600ee
-- wallet W2 wants to exchange 606ee for 200ff
-- W2 meets fitting offer from W1
-- W3 is a performer
-- W1 pays 200ff
-- W2 pays 606ee and gets 200ff
-- script holds 606ee
liquidityOrderSwap :: TestTree
liquidityOrderSwap = testCase "Liquidity Orders: W1 200ff -> 600ee fee 1/100 | Swap offers: W2 606ee -> 200ff" $ do
  performTest $ do
    h1 <- activateContractWallet (knownWallet 1) dexEndpoints
    h2 <- activateContractWallet (knownWallet 2) dexEndpoints
    h3 <- activateContractWallet (knownWallet 3) dexEndpoints

    void $ callEndpoint @"createLiquidityOrder" h1 (Request "a" 1 (LiquidityOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 600 (1,100)))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h2 (Request "b" 2 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 606 200))
    void $ waitNSlots 2

    void $ callEndpoint @"perform" h3 (Request "c" 3 ())
    void $ waitNSlots 2

    void $ callEndpoint @"collectFunds" h1 (Request "e" 5 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h2 (Request "f" 6 ())
    void $ waitNSlots 2

-- wallet W1 sets liquidity of 200ff and 600ee
-- wallet W2 wants to exchange 606ee for 200ff
-- W2 meets fitting offer from W1
-- W1 is a performer
-- W1 pays 200ff
-- W2 pays 606ee and gets 200ff
-- script holds 606ee
liquidityOrderSwapW1Performs :: TestTree
liquidityOrderSwapW1Performs = testCase "Liquidity Orders: W1 200ff -> 600ee fee 1/100 | Swap offers: W2 606ee -> 200ff" $ do
  performTest $ do
    h1 <- activateContractWallet (knownWallet 1) dexEndpoints
    h2 <- activateContractWallet (knownWallet 2) dexEndpoints

    void $ callEndpoint @"createLiquidityOrder" h1 (Request "a" 1 (LiquidityOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 600 (1,100)))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h2 (Request "b" 2 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 606 200))
    void $ waitNSlots 2

    void $ callEndpoint @"perform" h1 (Request "c" 3 ())
    void $ waitNSlots 2

    void $ callEndpoint @"collectFunds" h1 (Request "e" 5 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h2 (Request "f" 6 ())
    void $ waitNSlots 2

-- wallet W1 sets liquidity of 200ff and 600ee
-- wallet W2 wants to exchange 606ee for 200ff
-- W2 meets fitting offer from W1
-- W2 is a performer
-- W1 pays 200ff
-- W2 pays 606ee and gets 200ff
-- script holds 606ee
liquidityOrderSwapW2Performs :: TestTree
liquidityOrderSwapW2Performs = testCase "Liquidity Orders: W1 200ff -> 600ee fee 1/100 | Swap offers: W2 606ee -> 200ff" $ do
  performTest $ do
    h1 <- activateContractWallet (knownWallet 1) dexEndpoints
    h2 <- activateContractWallet (knownWallet 2) dexEndpoints

    void $ callEndpoint @"createLiquidityOrder" h1 (Request "a" 1 (LiquidityOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 600 (1,100)))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h2 (Request "b" 2 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 606 200))
    void $ waitNSlots 2

    void $ callEndpoint @"perform" h2 (Request "c" 3 ())
    void $ waitNSlots 2

    void $ callEndpoint @"collectFunds" h1 (Request "e" 5 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h2 (Request "f" 6 ())
    void $ waitNSlots 2

simpleLiquidityOrderSwapRace :: TestTree
simpleLiquidityOrderSwapRace = testCase "Liquidity Orders: W1 200ff -> 400ee 1/100 | Swap offers: W2 404ee -> 200ff, W3 404ee -> 200ff" $ do
  performTest $ do
    h1 <- activateContractWallet (knownWallet 1) dexEndpoints
    h2 <- activateContractWallet (knownWallet 2) dexEndpoints
    h3 <- activateContractWallet (knownWallet 3) dexEndpoints

    void $ callEndpoint @"createLiquidityOrder" h1 (Request "b" 1 (LiquidityOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 400 (1,100)))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h2 (Request "b" 2 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 650 200))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h3 (Request "b" 2 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 650 200))
    void $ waitNSlots 2

    void $ callEndpoint @"perform" h1 (Request "c" 3 ())
    void $ waitNSlots 2

    void $ callEndpoint @"collectFunds" h2 (Request "e" 5 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h3 (Request "e" 5 ())
    void $ waitNSlots 2

simpleLiquidityOrderSwapRaceWithSimpleSwap :: TestTree
simpleLiquidityOrderSwapRaceWithSimpleSwap = testCase "Liquidity Orders: W1 200ff -> 400ee (1,100) | Swap offers: W2 650ee -> 200ff, W3 650ee -> 200ff, W4 200ff -> 650ee" $ do
  performTest $ do
    h1 <- activateContractWallet (knownWallet 1) dexEndpoints
    h2 <- activateContractWallet (knownWallet 2) dexEndpoints
    h3 <- activateContractWallet (knownWallet 3) dexEndpoints
    h4 <- activateContractWallet (knownWallet 4) dexEndpoints

    void $ callEndpoint @"createLiquidityOrder" h1 (Request "a" 1 (LiquidityOrderParams (Value.AssetClass ("ff", "coin1")) (Value.AssetClass ("ee", "coin2")) 200 400 (1,100)))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h2 (Request "b" 2 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 650 200))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h3 (Request "c" 3 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 650 200))
    void $ waitNSlots 2
    void $ callEndpoint @"createSellOrder" h4 (Request "d" 4 (SellOrderParams (Value.AssetClass ("ee", "coin2")) (Value.AssetClass ("ff", "coin1")) 200 650))
    void $ waitNSlots 2

    void $ callEndpoint @"perform" h1 (Request "e" 5 ())
    void $ waitNSlots 2

    void $ callEndpoint @"collectFunds" h2 (Request "f" 6 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h3 (Request "g" 7 ())
    void $ waitNSlots 2
    void $ callEndpoint @"collectFunds" h4 (Request "h" 8 ())
    void $ waitNSlots 2

tests :: TestTree
tests = testGroup "Use cases" [ simpleSwap

                              , swapUnderExpectations
                              , swapUnderExpectationsW1Performs
                              , swapUnderExpectationsW2Performs

                              , swapAboveExpectations
                              , swapAboveExpectationsW1Performs
                              , swapAboveExpectationsW2Performs

                              , swapRace
                              , swapRaceW1Performs
                              , swapRaceW2Performs
                              , swapRaceW3Performs

                              , liquidityOrderSwap
                              , liquidityOrderSwapW1Performs
                              , liquidityOrderSwapW2Performs

                              , simpleLiquidityOrderSwapRaceWithSimpleSwap
                              , simpleLiquidityOrderSwapRace

                              ]

main :: IO ()
main = defaultMain tests
