{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Example trace for the uniswap contract
-}
module Uniswap.Trace
  ( setupTokens
  , tokenNames
  , wallets
  ) where

import           Control.Monad             (forM_, when)
import qualified Data.Semigroup            as Semigroup
import           Ledger
import           Ledger.Constraints        hiding (ownPubKeyHash)
import           Ledger.Value              as Value
import           Plutus.Contract           hiding (throwError)
import qualified Plutus.Contracts.Currency as Currency
import           Wallet.Emulator.Types     (Wallet, knownWallet, walletPubKeyHash)


-- | Create some sample tokens and distribute them to
--   the emulated wallets
setupTokens :: Contract (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
setupTokens = do
    ownPK <- ownPubKeyHash
    cur   <- Currency.mintContract ownPK [(tn, fromIntegral (length wallets) * amount) | tn <- tokenNames]
    let cs = Currency.currencySymbol cur
        v  = mconcat [Value.singleton cs tn amount | tn <- tokenNames]
    forM_ wallets $ \w -> do
        let pkh = walletPubKeyHash w
        Control.Monad.when (pkh /= ownPK) $ do
            tx <- submitTx $ mustPayToPubKey pkh v
            awaitTxConfirmed $ txId tx
    tell $ Just $ Semigroup.Last cur
  where
    amount = 1000000

wallets :: [Wallet]
wallets = [knownWallet i | i <- [1 .. 4]]

tokenNames :: [TokenName]
tokenNames = ["A", "B", "C", "D"]
