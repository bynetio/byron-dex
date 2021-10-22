{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

module Dex.OnChain
  ( mkDexValidator
  )
where

import           Dex.Types
import           Ledger
import           Ledger.Value     (assetClassValueOf)
import qualified PlutusTx
import           PlutusTx.Prelude

{-# INLINEABLE findOwnInput' #-}
findOwnInput' :: ScriptContext -> TxInInfo
findOwnInput' ctx = fromMaybe (error ()) (findOwnInput ctx)

{-# INLINEABLE mkDexValidator #-}
mkDexValidator ::
  DexDatum ->
  DexAction ->
  ScriptContext ->
  Bool
mkDexValidator (Order (SellOrder SellOrderInfo {..})) Swap ctx =
  traceIfFalse "correct payout utxo not found" (isJust payoutOutput)
  where
    txInfo = scriptContextTxInfo ctx
    txOutputs = txInfoOutputs txInfo
    payoutOutput = listToMaybe
      [ (txOut, PayoutInfo {..})
      | txOut <- txOutputs
      , Just (Payout (PayoutInfo ownerHash' orderId')) <- return $ do
              datumHash' <- txOutDatumHash txOut
              (Datum datum) <- findDatum datumHash' txInfo
              PlutusTx.fromBuiltinData datum
      , ownerHash == ownerHash'
      , orderId == orderId'
      , assetClassValueOf (txOutValue txOut) expectedCoin >= fromNat expectedAmount
      ]

mkDexValidator (Order (LiquidityOrder liquidityOrderInfo@LiquidityOrderInfo {..})) Swap ctx =
  traceIfFalse "correct payout utxo not found" (isJust payoutOutput) &&
  traceIfFalse "correct reversed liquidity order utxo not found" (isJust liquidityOrderOutput)
  where
    (numerator, denominator) = swapFee
    txInfo = scriptContextTxInfo ctx
    txOutputs = txInfoOutputs txInfo
    liquidityInput = findOwnInput' ctx
    liquidity = assetClassValueOf (txOutValue $ txInInfoResolved liquidityInput) lockedCoin
    payoutOutput = listToMaybe
      [ (txOut, PayoutInfo {..})
      | txOut <- txOutputs
      , Just (Payout (PayoutInfo ownerHash' orderId')) <- return $ do
              datumHash' <- txOutDatumHash txOut
              (Datum datum) <- findDatum datumHash' txInfo
              PlutusTx.fromBuiltinData datum
      , ownerHash == ownerHash'
      , orderId == orderId'
      , assetClassValueOf (txOutValue txOut) expectedCoin * fromNat denominator >= fromNat (expectedAmount * numerator)
      ]
    liquidityOrderOutput = listToMaybe
      [ (txOut, PayoutInfo {..})
      | txOut <- txOutputs
      , Just (Order (LiquidityOrder liquidityOrderInfo')) <- return $ do
              datumHash' <- txOutDatumHash txOut
              (Datum datum) <- findDatum datumHash' txInfo
              PlutusTx.fromBuiltinData datum
      , liquidityOrderInfo == reversedLiquidityOrder liquidity liquidityOrderInfo'
      , assetClassValueOf (txOutValue txOut) expectedCoin >= fromNat expectedAmount
      ]

mkDexValidator (Order o) CancelOrder ctx =
  traceIfFalse "Not signed by owner" checkSignatories
  where
    ownerHash' = case o of
      LiquidityOrder LiquidityOrderInfo {..} -> ownerHash
      SellOrder SellOrderInfo {..}           -> ownerHash
    txInfo = scriptContextTxInfo ctx
    signs = txInfoSignatories txInfo
    checkSignatories = ownerHash' `elem` signs


mkDexValidator (Payout PayoutInfo {..}) CollectCoins ctx =
  traceIfFalse "Not signed by owner" checkSignatories
  where
    txInfo = scriptContextTxInfo ctx
    signs = txInfoSignatories txInfo
    checkSignatories = ownerHash `elem` signs

mkDexValidator _ _ _ = False
