{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Dex.OffChain
where

import           Control.Applicative       ((<|>))
import           Control.Monad             hiding (fmap, mapM, mapM_)
import           Control.Monad.Freer
import           Data.List                 (scanl)
import qualified Data.Map                  as Map
import           Data.Proxy                (Proxy (..))
import           Data.Text                 (Text, pack)
import           Data.Void                 (Void, absurd)
import           Dex.OnChain               (mkDexValidator)
import           Dex.Types
import qualified GHC.Classes
import           GHC.TypeLits              (symbolVal)
import           Ledger                    hiding (fee, singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              (assetClassValue, assetClassValueOf,
                                            getValue)
import           Playground.Contract
import           Plutus.Contract
import qualified Plutus.Contracts.Currency as Currency
import qualified PlutusTx
import qualified PlutusTx.AssocMap         as AssocMap
import           PlutusTx.Prelude          hiding (Semigroup (..), unless)
import           Prelude                   (Semigroup (..), String, div, show)
import           Text.Printf               (printf)

data Uniswapping

instance Scripts.ValidatorTypes Uniswapping where
  type RedeemerType Uniswapping = DexAction
  type DatumType Uniswapping = DexDatum



dexInstance :: Scripts.TypedValidator Uniswapping
dexInstance =
  Scripts.mkTypedValidator @Uniswapping
    ( $$(PlutusTx.compile [||mkDexValidator||])
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @DexDatum @DexAction



type DexSchema =
  Endpoint "sell" SellOrderParams
  .\/ Endpoint "perform" ()


sell :: SellOrderParams -> Contract () DexSchema Text ()
sell SellOrderParams {..} = do
  ownerHash <- pubKeyHash <$> ownPubKey
  let order = SellOrder (SellOrderInfo coinIn coinOut ratio ownerHash)
  let tx = Constraints.mustPayToTheScript order (assetClassValue coinIn amountIn)
  void $ submitTxConstraints dexInstance tx




perform :: Contract () DexSchema Text ()
perform = do
  pkh <- pubKeyHash <$> ownPubKey
  let address = Ledger.scriptAddress $ Scripts.validatorScript dexInstance
  utxos <- Map.toList <$> utxoAt address
  mapped <- mapM (\(oref, o) -> getOrderDatum o >>= \d -> return (o, oref, d)) utxos
  let lookups = Constraints.typedValidatorLookups dexInstance
          <> Constraints.ownPubKeyHash pkh
          <> Constraints.otherScript (Scripts.validatorScript dexInstance)
          <> Constraints.unspentOutputs (Map.fromList utxos)

      tx = foldl (\acc (o, oref, SellOrder SellOrderInfo {..}) ->
        let inValue = assetClassValueOf (txOutValue $ txOutTxOut o) coinIn
            (numerator, denominator) = ratio
            outValue = (inValue * numerator `div` denominator)

        in acc <> Constraints.mustPayToPubKey ownerHash (assetClassValue coinOut outValue)
              <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Perform)
        ) mempty mapped
  void $ submitTxConstraintsWith lookups tx


getOrderDatum :: TxOutTx -> Contract w s Text DexDatum
getOrderDatum o = case txOutDatumHash $ txOutTxOut o of
  Nothing -> throwError "datumHash not found"
  Just h -> case Map.lookup h $ txData $ txOutTxTx o of
    Nothing -> throwError "datum not found"
    Just (Datum e) -> case PlutusTx.fromBuiltinData e of
      Nothing -> throwError "datum has wrong type"
      Just d  -> return d

dexEndpoint :: Promise () DexSchema Text ()
dexEndpoint = (handleEndpoint @"sell" @_ @_ @_ @Text (\case
      (Right p) ->  sell p
      Left _    -> return ())
    `select` handleEndpoint @"perform" @_ @_ @_ @Text (const perform)) <> dexEndpoint
