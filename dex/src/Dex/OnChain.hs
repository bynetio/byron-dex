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
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

module Dex.OnChain
  ( mkDexValidator
  )
where

import           Dex.Types
import           Ledger
-- import           Ledger.Value     (assetClassValueOf)
-- import qualified PlutusTx
import           PlutusTx.Prelude




-- {-# INLINABLE lookup #-}
-- lookup :: (Eq a) => a -> [(a,b)] -> Maybe b
-- lookup _ [] = Nothing
-- lookup !a ((!a',!b) : (!as)) | a == a' = Just b
--                          | otherwise = lookup a as





-- {-# INLINABLE containsPayout #-}
-- containsPayout ::
--   BuiltinByteString ->
--   PaymentPubKeyHash ->
--   AssetClass ->
--   Integer ->
--   [TxOut] ->
--   [(DatumHash, Datum)] ->
--   Bool
-- containsPayout _ _ _ _ [] _ = False
-- containsPayout orderId ownerHash expectedCoin expectedAmount (txOut:txOuts) txData =
--   case txOutDatumHash txOut of
--     Just datumHash' -> case lookup datumHash' txData of
--       Just (Datum datum) -> case PlutusTx.fromBuiltinData datum of
--         Just (Payout (PayoutInfo ownerHash' orderId')) ->
--           (ownerHash' == ownerHash && orderId' == orderId && assetClassValueOf (txOutValue txOut) expectedCoin >=  expectedAmount)
--             || containsPayout orderId ownerHash expectedCoin expectedAmount txOuts txData
--         _ -> containsPayout orderId ownerHash expectedCoin expectedAmount txOuts  txData
--       _ -> containsPayout orderId ownerHash expectedCoin expectedAmount txOuts  txData
--     _ -> containsPayout orderId ownerHash expectedCoin expectedAmount txOuts txData


{-# INLINABLE mkDexValidator #-}
-- removing all validation and returning True removes all mkDexValidator memory usage
-- see: mem-no-validation.svg
-- mkDexValidator _ _ _ = True
mkDexValidator ::
  DexDatum ->
  DexAction ->
  ScriptContext ->
  Bool
-- mkDexValidator (Order (SellOrder SellOrderInfo {..})) Swap ctx =
--   traceIfFalse "correct payout utxo not found" (isJust payoutOutput)
--   where
--     txInfo = scriptContextTxInfo ctx
--     txOutputs = getContinuingOutputs ctx
--     payoutOutput = listToMaybe
--       [ txOut
--       | txOut <- txOutputs
--       , Just (Payout (PayoutInfo ownerHash' orderId')) <- return $ do
--               datumHash' <- txOutDatumHash txOut
--               (Datum datum) <- lookup datumHash' (txInfoData txInfo)
--               PlutusTx.fromBuiltinData datum
--       , ownerHash == ownerHash'
--       , orderId == orderId'
--       , assetClassValueOf (txOutValue txOut) expectedCoin >= fromNat expectedAmount
--       ]

-- mkDexValidator (Order (LiquidityOrder liquidityOrderInfo@LiquidityOrderInfo {..})) Swap ctx =
--   traceIfFalse "correct reversed liquidity order utxo not found" (isJust liquidityOrderOutput)
--   where
--     (numerator, denominator) = swapFee
--     txInfo = scriptContextTxInfo ctx
--     txOutputs = getContinuingOutputs ctx
--     liquidityOrderOutput = listToMaybe
--       [ txOut
--       | txOut <- txOutputs
--       , Just (Order (LiquidityOrder liquidityOrderInfo')) <- return $ do
--               datumHash' <- txOutDatumHash txOut
--               (Datum datum) <- lookup datumHash' (txInfoData txInfo)
--               PlutusTx.fromBuiltinData datum
--       , liquidityOrderInfo == reversedLiquidityOrder (fromNat expectedAmount) liquidityOrderInfo'
--       , assetClassValueOf (txOutValue txOut) expectedCoin * fromNat denominator >= fromNat (expectedAmount * (denominator + numerator))
--       ]
mkDexValidator (Order _) Swap _ = True

mkDexValidator (Order o) CancelOrder ctx =
  traceIfFalse "Not signed by owner" checkSignatories
  where
    ownerHash' = case o of
      LiquidityOrder LiquidityOrderInfo {..} -> ownerHash
      SellOrder SellOrderInfo {..}           -> ownerHash
    txInfo = scriptContextTxInfo ctx
    signs = txInfoSignatories txInfo
    checkSignatories = unPaymentPubKeyHash ownerHash' `elem` signs


mkDexValidator (Payout PayoutInfo {..}) CollectCoins ctx =
  traceIfFalse "Not signed by owner" checkSignatories
  where
    txInfo = scriptContextTxInfo ctx
    signs = txInfoSignatories txInfo
    checkSignatories = unPaymentPubKeyHash ownerHash `elem` signs

mkDexValidator _ _ _ = False
