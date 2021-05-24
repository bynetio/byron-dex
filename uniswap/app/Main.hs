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


main :: IO ()
main = runEmulatorTraceIO' def emulatorCfg myTrace
  where
    emulatorCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet i, v) | i <- [1..10]]
      where
	v = Ada.lovelaceValueOf 100_000_000 <>
	    Value.singleton customSymbol customToken 100_000_000




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
        void $ waitNSlots 1


  where
    userTrace :: Uniswap -> EmulatorTrace ()
    userTrace pool = do
        h2 <- activateContractWallet (Wallet 2) $ userEndpoints pool
        h3 <- activateContractWallet (Wallet 3) $ userEndpoints pool
        void $ callEndpoint @"create" h2 (CreateParams (Coin $ Value.assetClass customSymbol customToken) (Coin $ Value.assetClass "" "") 1000 1000)
        return ()

myReverse :: [a] -> [a]
myReverse = id
