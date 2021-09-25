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
import           Ledger.Address                   (pubKeyHashAddress)
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import           Ledger.Value                     (AssetClass (..),
                                                   assetClassValueOf, symbols)
import qualified PlutusTx
import           PlutusTx.Prelude

{-# INLINEABLE findOwnInput' #-}
findOwnInput' :: ScriptContext -> TxInInfo
findOwnInput' ctx = fromMaybe (error ()) (findOwnInput ctx)

{-# INLINEABLE valueWithin #-}
valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

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
    inputs = map txInInfoResolved $ txInfoInputs txInfo
    expectedAmount = sum
      [ (assetClassValueOf (txOutValue o) coinIn * numerator) `divide` denominator
      | o <- inputs
      --, txOutAddress o == pubKeyHashAddress ownerHash
      , let order = txOutDatumHash o >>= \datumHash -> findDatum datumHash txInfo >>= \(Datum bd) -> PlutusTx.fromBuiltinData bd
      , isJust order
      , let Just (SellOrder SellOrderInfo {..}) = order
      , let (numerator, denominator) = ratio
      ]

    (numerator, denominator) = ratio
    ownInput = findOwnInput' ctx
    inVal = valueWithin ownInput

    outAmount :: Integer
    outAmount = sum [ assetClassValueOf o coinOut
                       | o <- pubKeyOutputsAt ownerHash $ scriptContextTxInfo ctx
                    ]
