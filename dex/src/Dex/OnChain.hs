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
  traceIfFalse "zero input" (expectedAmount > 0) &&
  traceIfFalse "not enough coins given" (expectedAmount <= outAmount )
  where
    txInfo = scriptContextTxInfo ctx
    txInputs = map txInInfoResolved $ txInfoInputs txInfo
    expectedAmount = sum
      [ (assetClassValueOf (txOutValue o) coinIn * fromNat numerator) `divide` fromNat denominator
      | o <- txInputs
      --, txOutAddress o == pubKeyHashAddress ownerHash
      , let order = txOutDatumHash o >>= \datumHash' -> findDatum datumHash' txInfo >>= \(Datum bd) -> PlutusTx.fromBuiltinData bd
      , isJust order
      , let Just (SellOrder SellOrderInfo {ratio=(numerator,denominator)}) = order
      ]

    outAmount :: Integer
    outAmount = sum [ assetClassValueOf o coinOut
                       | o <- pubKeyOutputsAt ownerHash $ scriptContextTxInfo ctx
                    ]
