{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Main where

import           Control.Monad.Freer.Extras   as Extras
import           Data.Default
import           Data.Functor                 (void)
import qualified Data.Map                     as Map
import           Data.Monoid
import           Data.Text                    (Text)
import           Data.Void                    (Void)
import           Plutus.Trace.Emulator        as Emulator
import qualified Plutus.V1.Ledger.Ada         as Ada
import qualified Plutus.V1.Ledger.Value       as Value
import           Uniswap.OffChain
import           Uniswap.Types
import           Wallet.Emulator.Wallet       as Wallet

import           Uniswap.Common.WalletHistory (History, HistoryId)
import qualified Uniswap.Common.WalletHistory as WH

customSymbol :: [Char]
customSymbol = "ff"

customToken :: [Char]
customToken = "PLN"

customSymbolsAndTokens :: [(Value.CurrencySymbol, Value.TokenName)]
customSymbolsAndTokens = [("ff", "coin1"), ("ee", "coin2"), ("dd", "coin3"), ("cc", "coin4"), ("bb", "coin5")]

customCoin1, customCoin2, customCoin3, customCoin4, customCoin5 :: Coin a
[customCoin1, customCoin2, customCoin3, customCoin4, customCoin5] = map (Coin . uncurry Value.assetClass) customSymbolsAndTokens

customSymbol2 :: [Char]
customSymbol2 = "ee"

customToken2 :: [Char]
customToken2 = "BTC"

customSymbol3 :: [Char]
customSymbol3 = "dd"

customToken3 :: [Char]
customToken3 = "ETH"

main :: IO ()
main = return ()

runTrace :: EmulatorTrace () -> IO ()
runTrace = runEmulatorTraceIO' def emulatorCfg
  where
    emulatorCfg = EmulatorConfig $ Left $ Map.fromList ([(Wallet i, v) | i <- [1 .. 4]])
      where
        v = Ada.lovelaceValueOf 100_000_000 <> mconcat (map (\(symbol,tokenName) -> Value.singleton symbol tokenName 100_000_000) customSymbolsAndTokens)

iswap :: IO ()
iswap = runTrace iswapTrace

slippage :: IO ()
slippage = runTrace slippageTrace

customFees :: IO ()
customFees = runTrace customFeesTrace

mixed :: IO ()
mixed = runTrace mixedTrace

adaCoin :: Coin a
adaCoin = Coin $ Value.assetClass "" ""


iswapTrace :: EmulatorTrace ()
iswapTrace = do
  h1 <- activateContractWallet (Wallet 1) ownerEndpoint
  void $ callEndpoint @"start" h1 (WithHistoryId "guid1" ())
  void $ waitNSlots 10
  maybePool <- WH.lookup "guid1" <$> observableState h1
  case maybePool of
    Just (Right pool) -> userTrace pool
    _                 -> return ()
  void $ waitNSlots 10
  where
    userTrace :: Uniswap -> EmulatorTrace ()
    userTrace pool = do
        h2 <- activateContractWallet (Wallet 2) $ userEndpoints pool
        h4 <- activateContractWallet (Wallet 4) $ userEndpoints pool
        void $ callEndpoint @"create" h2 (WithHistoryId "" $ CreateParams customCoin2 customCoin1 (3,1000) 100 500)
        void $ waitNSlots 1
        void $ callEndpoint @"create" h2 (WithHistoryId "" $ CreateParams customCoin3 customCoin1 (3,1000) 1000 1000)
        void $ waitNSlots 1

        void $ callEndpoint @"iSwapPreview" h4 (WithHistoryId "guid2" $ ISwapPreviewParams customCoin1 customCoin3 500)
        void $ waitNSlots 5
        maybePreview <- WH.lookup "guid2" <$> observableState h4
        case maybePreview of
          Just (Right (ISwapPreview ((ca,a),(cb,b)))) ->
            void $ callEndpoint @"iSwap" h4 (WithHistoryId "" $ IndirectSwapParams ca cb a b 0)
          _ ->
            return ()


slippageTrace :: EmulatorTrace ()
slippageTrace = do
  h1 <- activateContractWallet (Wallet 1) ownerEndpoint
  void $ callEndpoint @"start" h1 (WithHistoryId "guid3" ())
  void $ waitNSlots 10
  maybePool <- WH.lookup "guid3" <$> observableState h1
  case maybePool of
    Just (Right pool) -> userTrace pool
    _                 -> return ()
  void $ waitNSlots 10
  where
    userTrace :: Uniswap -> EmulatorTrace ()
    userTrace pool = do
        h2 <- activateContractWallet (Wallet 2) $ userEndpoints pool
        h3 <- activateContractWallet (Wallet 3) $ userEndpoints pool
        void $ callEndpoint @"create" h2 (WithHistoryId "" $ CreateParams customCoin2 customCoin1 (3,1000) 100 500)
        void $ waitNSlots 1
        void $ callEndpoint @"create" h2 (WithHistoryId "" $ CreateParams customCoin3 customCoin1 (3,1000) 1000 1000)
        void $ waitNSlots 1
        void $ callEndpoint @"iSwapPreview" h3 (WithHistoryId "guid4" $ ISwapPreviewParams customCoin1 customCoin3 500)
        void $ waitNSlots 5

        -- void $ callEndpoint @"iSwapPreview" h4 (ISwapPreviewParams customCoin1 customCoin3 500)
        -- void $ waitNSlots 5
        -- maybePreview <- getLast <$> observableState h4
        -- case maybePreview of
        --   Just (Right (ISwapPreview ((ca,a),(cb,b)))) ->
        --     void $ callEndpoint @"iSwap" h4 (IndirectSwapParams ca cb a b 5)
        --   _ ->
        --     return ()

        maybePreview <- WH.lookup "guid4" <$> observableState h3
        case maybePreview of
          Just (Right (ISwapPreview ((ca,a),(cb,b)))) ->
            void $ callEndpoint @"iSwap" h3 (WithHistoryId "" $ IndirectSwapParams ca cb a b 5)
          _ ->
            return ()


customFeesTrace :: EmulatorTrace ()
customFeesTrace = do
  h1 <- activateContractWallet (Wallet 1) ownerEndpoint
  void $ callEndpoint @"start" h1 (WithHistoryId "guid5" ())
  void $ waitNSlots 10
  maybePool <- WH.lookup "guid5" <$> observableState h1
  case maybePool of
    Just (Right pool) -> userTrace pool
    _                 -> return ()
  void $ waitNSlots 10
  where
    userTrace :: Uniswap -> EmulatorTrace ()
    userTrace pool = do
        h2 <- activateContractWallet (Wallet 2) $ userEndpoints pool
        h3 <- activateContractWallet (Wallet 3) $ userEndpoints pool
        void $ callEndpoint @"create" h2 (WithHistoryId "" $ CreateParams customCoin2 customCoin1 (3,1000) 100 500)
        void $ waitNSlots 1
        void $ callEndpoint @"create" h2 (WithHistoryId "" $ CreateParams customCoin3 customCoin1 (3,1000) 1000 1000)
        void $ waitNSlots 1

        void $ callEndpoint @"iSwap" h3 (WithHistoryId "" $ IndirectSwapParams customCoin1 customCoin3 500 333 5)
        void $ waitNSlots 5

        void $ callEndpoint @"close" h2 (WithHistoryId "" $ CloseParams customCoin2 customCoin1 (3,1000))
        void $ waitNSlots 1
        void $ callEndpoint @"close" h2 (WithHistoryId "" $ CloseParams customCoin3 customCoin1 (3,1000))
        void $ waitNSlots 1


mixedTrace :: EmulatorTrace ()
mixedTrace = do
  h1 <- activateContractWallet (Wallet 1) ownerEndpoint
  void $ callEndpoint @"start" h1 (WithHistoryId "guid6" ())
  void $ waitNSlots 10
  maybePool <- WH.lookup "guid6" <$> observableState h1
  case maybePool of
    Just (Right pool) -> userTrace pool
    _                 -> return ()
  void $ waitNSlots 10
  where
    userTrace :: Uniswap -> EmulatorTrace ()
    userTrace pool = do
        h2 <- activateContractWallet (Wallet 2) $ userEndpoints pool
        h3 <- activateContractWallet (Wallet 3) $ userEndpoints pool
        h4 <- activateContractWallet (Wallet 4) $ userEndpoints pool
        let fee = (3,1000)
        void $ callEndpoint @"create" h2 (WithHistoryId "" $ CreateParams customCoin2 customCoin1 fee 100 500)
        void $ waitNSlots 1
        void $ callEndpoint @"create" h4 (WithHistoryId "" $ CreateParams customCoin3 customCoin1 fee 1000 1000)
        void $ waitNSlots 1
        callEndpoint @"swapPreview" h3 (WithHistoryId "guid7" $ SwapPreviewParams customCoin1 customCoin2 fee 500)
        maybePreview <- getContractState' "guid7" h3
        logInfo $ "swapPreview: " ++ show maybePreview
        let Right (SwapPreview (_, (_, amount), _)) = maybePreview
        callEndpoint @"swap" h3 (WithHistoryId "" $ SwapParams customCoin1 customCoin2 fee 500 amount 1)
        void $ waitNSlots 10
        callEndpoint @"iSwapPreview" h3 (WithHistoryId "guid8" $ ISwapPreviewParams customCoin1 customCoin3 600)
        void $ waitNSlots 10
        maybeISwapPreview <- getContractState' "guid8" h3
        logInfo $ "iSwapPreview: " ++ show maybeISwapPreview
        let Right (ISwapPreview (_, (_, amount'))) = maybeISwapPreview
        callEndpoint @"iSwap" h3 (WithHistoryId "" $ IndirectSwapParams customCoin1 customCoin3 (Amount 600) amount' 1)
        void $ callEndpoint @"add" h4 (WithHistoryId "" $ AddParams customCoin2 customCoin1 fee 1000 1000)
        void $ waitNSlots 10
        void $ callEndpoint @"close" h2 (WithHistoryId "" $ CloseParams customCoin2 customCoin1 fee)
        void $ waitNSlots 10

getContractState' :: HistoryId -> ContractHandle (History (Either Text UserContractState)) UniswapUserSchema Void -> EmulatorTrace (Either Text UserContractState)
getContractState' guid h = do
    void $ waitNSlots 1
    go 10
  where
    go :: Int -> EmulatorTrace (Either Text UserContractState)
    go 0 = return $ Left "Can't get state"
    go n = do
      maybeState <- WH.lookup guid <$> observableState h
      case maybeState of
        Just s -> return s
        Nothing -> do
          void $ waitNSlots 1
          go (n - 1)
