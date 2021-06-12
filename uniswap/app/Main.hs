{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Main where
import           Uniswap.OffChain
import           Uniswap.Types

import           Control.Monad.Freer.Extras as Extras
import           Data.Default
import           Data.Functor               (void)
import qualified Data.Map                   as Map
import           Data.Monoid
import           Ledger
import           Plutus.Trace.Emulator      as Emulator
import qualified Plutus.V1.Ledger.Ada       as Ada
import qualified Plutus.V1.Ledger.Value     as Value
import           Wallet.Emulator.Wallet     as Wallet
customSymbol = "ff"
customToken = "PLN"

customSymbolsAndTokens = [("ff","coin1"),("ee","coin2"),("dd","coin3"),("cc","coin4"),("bb","coin5")]
[customCoin1,customCoin2,customCoin3,customCoin4,customCoin5] = map (Coin . uncurry Value.assetClass) customSymbolsAndTokens
customSymbol2 = "ee"
customToken2 = "BTC"

customSymbol3 = "dd"
customToken3 = "ETH"



main :: IO ()
main = runEmulatorTraceIO' def emulatorCfg myTrace
  where
    emulatorCfg = EmulatorConfig $ Left $ Map.fromList ([(Wallet i, v) | i <- [1..4]])
      where
        v = Ada.lovelaceValueOf 100_000_000 <> mconcat (map (\(symbol,tokenName) -> Value.singleton symbol tokenName 100_000_000) customSymbolsAndTokens)



adaCoin = Coin $ Value.assetClass "" ""


myTrace :: EmulatorTrace ()
myTrace = do
  h1 <- activateContractWallet (Wallet 1) ownerEndpoint
  void $ callEndpoint @"start" h1 ()
  void $ waitNSlots 10
  maybePool <- getLast <$> observableState h1
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
        void $ callEndpoint @"create" h2 (CreateParams customCoin1 customCoin2  1000 1000)
        void $ waitNSlots 1
        void $ callEndpoint @"create" h2 (CreateParams customCoin2 customCoin3  1000 1000)
        void $ waitNSlots 1
        void $ callEndpoint @"create" h2 (CreateParams customCoin1 customCoin4 1_000_000 1_000_000)
        void $ waitNSlots 1
        void $ callEndpoint @"create" h2 (CreateParams customCoin4 customCoin3 10_000 10_000)
        void $ waitNSlots 1

        void $ callEndpoint @"iSwap" h3 (IndirectSwapParams customCoin1 customCoin3 0 500)
        void $ waitNSlots 1
        void $ callEndpoint @"add" h4 (AddParams customCoin1 customCoin2 1000 1000)
        void $ waitNSlots 10
        void $ callEndpoint @"remove" h4 (RemoveParams customCoin1 customCoin2 1040)
        void $ waitNSlots 10
        -- void $ callEndpoint @"close" h2 (CloseParams customCoin1 customCoin2 )

myReverse :: [a] -> [a]
myReverse = id
