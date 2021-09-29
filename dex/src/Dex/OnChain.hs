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


{-# INLINEABLE mkDexValidator #-}
mkDexValidator ::
  DexDatum ->
  DexAction ->
  ScriptContext ->
  Bool
mkDexValidator (SellOrder SellOrderInfo {..}) Perform ctx =
  traceIfFalse "zero input" (fromNat expectedTotalAmount > 0) &&
  traceIfFalse "not enough coins given" (fromNat expectedTotalAmount <= outAmount )
  where
    txInfo = scriptContextTxInfo ctx
    txInputs = map txInInfoResolved $ txInfoInputs txInfo
    expectedTotalAmount = sum
      [ expectedAmount
      | o <- txInputs
      --, txOutAddress o == pubKeyHashAddress ownerHash
      , let order = txOutDatumHash o >>= \datumHash' -> findDatum datumHash' txInfo >>= \(Datum bd) -> PlutusTx.fromBuiltinData bd
      , isJust order
      , let Just (SellOrder SellOrderInfo {..}) = order
      ]

    outAmount :: Integer
    outAmount = sum [ assetClassValueOf o coinOut
                       | o <- pubKeyOutputsAt ownerHash $ scriptContextTxInfo ctx
                    ]
