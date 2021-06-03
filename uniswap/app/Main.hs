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

customSymbol2 = "ee"
customToken2 = "BTC"

customSymbol3 = "dd"
customToken3 = "ETH"

main :: IO ()
main = runEmulatorTraceIO' def emulatorCfg myTrace
  where
    emulatorCfg = EmulatorConfig $ Left $ Map.fromList ([(Wallet i, v) | i <- [1..10]] ++ [(Wallet 11, Ada.lovelaceValueOf  100_000_000 <> Value.singleton  customSymbol customToken 100_000_000)])
      where
        v = Value.singleton customSymbol customToken 100_000_000 <>
            Value.singleton customSymbol2 customToken2 100_000_000 <>
            Value.singleton customSymbol3 customToken3 100_000_000 <>
            Ada.lovelaceValueOf 100_000_000


customCoin = Coin $ Value.assetClass customSymbol customToken
customCoin2 = Coin $ Value.assetClass customSymbol2 customToken2
customCoin3 = Coin $ Value.assetClass customSymbol3 customToken3
adaCoin = Coin $ Value.assetClass "" ""


myTrace :: EmulatorTrace ()
myTrace = do
	h1 <- activateContractWallet (Wallet 1) ownerEndpoint
        void $ callEndpoint @"start" h1 ()
        Extras.logInfo @String "przed"
        void $ waitNSlots 10
        Extras.logInfo @String "miedzy"
        maybePool <- getLast <$> observableState h1
        Extras.logInfo @String (show maybePool)
        case maybePool of
	  Just (Right pool) -> userTrace pool
          _                 -> return ()
        void $ waitNSlots 10


  where
    userTrace :: Uniswap -> EmulatorTrace ()
    userTrace pool = do
        h2 <- activateContractWallet (Wallet 2) $ userEndpoints pool
        h11 <- activateContractWallet (Wallet 11) $ userEndpoints pool
        h4 <- activateContractWallet (Wallet 4) $ userEndpoints pool
        void $ callEndpoint @"create" h2 (CreateParams customCoin customCoin2  1000 1000)
        void $ waitNSlots 10
        void $ callEndpoint @"create" h2 (CreateParams customCoin2 customCoin3  1000 1000)
        void $ waitNSlots 10
        void $ callEndpoint @"iSwap" h11 (IndirectSwapParams customCoin customCoin3 500 0)
        void $ waitNSlots 100
        -- void $ callEndpoint @"add" h4 (AddParams customCoin customCoin2 1000 1000)
        -- void $ waitNSlots 10
        -- void $ callEndpoint @"remove" h4 (RemoveParams customCoin customCoin2 1040)
        -- void $ waitNSlots 10
        -- void $ callEndpoint @"close" h2 (CloseParams customCoin customCoin2 )

myReverse :: [a] -> [a]
myReverse = id
