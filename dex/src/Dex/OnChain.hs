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
  traceIfFalse "not enough coins given" (inAmount * numerator <= outAmount * denominator)
  where
    (numerator, denominator) = ratio
    ownInput = findOwnInput' ctx
    inVal = valueWithin ownInput
    inAmount = assetClassValueOf inVal coinIn

    outAmount :: Integer
    outAmount = sum [ assetClassValueOf o coinOut
                       | o <- pubKeyOutputsAt ownerHash $ scriptContextTxInfo ctx
                    ]
