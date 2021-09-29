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

import           Control.Monad        hiding (fmap, mapM, mapM_)
import           Data.List            (foldl', sortOn)
import qualified Data.Map             as Map
import           Data.Ord             (comparing)
import           Data.Proxy           (Proxy (..))
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Dex.OnChain          (mkDexValidator)
import           Dex.Types
import           Dex.WalletHistory    as WH
import qualified GHC.Classes
import           GHC.TypeLits         (symbolVal)
import           Ledger               hiding (fee, singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         (AssetClass (..), assetClassValue,
                                       assetClassValueOf, getValue)
import           Playground.Contract
import           Plutus.Contract
import qualified PlutusTx
import qualified PlutusTx.AssocMap    as AssocMap
import           PlutusTx.Prelude     hiding (Semigroup (..), unless)
import           Prelude              (Semigroup (..), fromIntegral, (/))
data Uniswapping

instance Scripts.ValidatorTypes Uniswapping where
  type RedeemerType Uniswapping = DexAction
  type DatumType Uniswapping = DexDatum



dexInstance :: Scripts.TypedValidator Uniswapping
dexInstance =
  Scripts.mkTypedValidator @Uniswapping
    ( $$(PlutusTx.compile [||mkDexValidator||]))
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @DexDatum @DexAction



type DexSchema =
  Endpoint "sell" (WithHistoryId SellOrderParams)
  .\/ Endpoint "perform" (WithHistoryId ())
  .\/ Endpoint "stop" (WithHistoryId ())
  .\/ Endpoint "funds" (WithHistoryId ())
  .\/ Endpoint "findOrders" (WithHistoryId ())

sell :: SellOrderParams -> Contract (History (Either Text DexContractState)) DexSchema Text ()
sell SellOrderParams {..} = do
  ownerHash <- pubKeyHash <$> ownPubKey

  let address = Ledger.scriptAddress $ Scripts.validatorScript dexInstance
  utxos <- Map.toList <$> utxoAt address
  mapped <- mapM (\(oref, o) -> getOrderDatum o >>= \d -> return (o, oref, d)) utxos
  let filtered =
        [ (oref, o, d)
        | (oref, o, d@(SellOrder (SellOrderInfo coinIn' coinOut' _ _))) <- mapped
        , coinIn == coinOut'
        , coinOut == coinIn'
        ]
  let sorted = sortOn (\(o,_,SellOrder SellOrderInfo {coinIn=coinIn', expectedAmount=expectedAmount'}) ->
                                  let amountIn' = assetClassValueOf (txOutValue $ txOutTxOut o) coinIn'
                                  in fromIntegral (fromNat expectedAmount') / fromIntegral amountIn') filtered


  let (utxosToCollect, _, reward) =
        foldl' (\(utxosAcc, moneyToSpend, rewardAcc) order@(o,oref,SellOrder (SellOrderInfo coinIn' coinOut' cost _)) ->
          let reward = assetClassValueOf (txOutValue $ txOutTxOut o) coinIn'
          in if moneyToSpend - fromNat cost < 0
            then (utxosAcc, moneyToSpend, rewardAcc)
            else (order:utxosAcc, moneyToSpend - fromNat cost, rewardAcc + reward)
          ) ([], fromNat amountIn, 0) sorted
  if reward >= fromNat expectedAmount
    then do
      let tx = foldl' (\acc (o, oref, SellOrder SellOrderInfo {..}) ->
                let inValue = assetClassValueOf (txOutValue $ txOutTxOut o) coinIn

                in acc <> Constraints.mustPayToPubKey ownerHash (assetClassValue coinOut (fromNat expectedAmount))
                      <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Perform)
                ) mempty utxosToCollect
      let lookups = Constraints.typedValidatorLookups dexInstance
            <> Constraints.ownPubKeyHash ownerHash
            <> Constraints.otherScript (Scripts.validatorScript dexInstance)
            <> Constraints.unspentOutputs (Map.fromList (map (\(o,oref,_) -> (oref,o)) utxosToCollect))
      void $ submitTxConstraintsWith lookups tx
    else do
      let order = SellOrder (SellOrderInfo coinIn coinOut expectedAmount ownerHash)
      let tx = Constraints.mustPayToTheScript order (assetClassValue coinIn (fromNat amountIn))
      void $ submitTxConstraints dexInstance tx


findOrders :: Contract (History (Either Text DexContractState)) DexSchema Text [(SellOrderInfo, TxOutRef)]
findOrders = do
  let address = Ledger.scriptAddress $ Scripts.validatorScript dexInstance
  utxos <- Map.toList <$> utxoAt address
  mapM (\(oref, o) -> getOrderDatum o >>= \case
    SellOrder sellOrder -> return (sellOrder, oref)
    ) utxos


perform :: Contract (History (Either Text DexContractState)) DexSchema Text ()
perform = do
  pkh <- pubKeyHash <$> ownPubKey
  let address = Ledger.scriptAddress $ Scripts.validatorScript dexInstance
  utxos <- Map.toList <$> utxoAt address
  mapped <- mapM (\(oref, o) -> getOrderDatum o >>= \d -> return (o, oref, d)) utxos
  let lookups = Constraints.typedValidatorLookups dexInstance
          <> Constraints.ownPubKeyHash pkh
          <> Constraints.otherScript (Scripts.validatorScript dexInstance)
          <> Constraints.unspentOutputs (Map.fromList utxos)
      tx = foldl' (\acc (o, oref, SellOrder SellOrderInfo {..}) ->
        let inValue = assetClassValueOf (txOutValue $ txOutTxOut o) coinIn

        in acc <> Constraints.mustPayToPubKey ownerHash (assetClassValue coinOut (fromNat expectedAmount))
              <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Perform)
        ) mempty mapped
  void $ submitTxConstraintsWith lookups tx


funds :: Contract w s Text [(AssetClass, Integer)]
funds = do
  pkh <- pubKeyHash <$> ownPubKey
  os <- map snd . Map.toList <$> utxoAt (pubKeyHashAddress pkh)
  let walletValue = getValue $ mconcat [txOutValue $ txOutTxOut o | o <- os]
  return [(AssetClass (cs, tn),  a) | (cs, tns) <- AssocMap.toList walletValue, (tn, a) <- AssocMap.toList tns]



getOrderDatum :: TxOutTx -> Contract w s Text DexDatum
getOrderDatum o = case txOutDatumHash $ txOutTxOut o of
  Nothing -> throwError "datumHash not found"
  Just h -> case Map.lookup h $ txData $ txOutTxTx o of
    Nothing -> throwError "datum not found"
    Just (Datum e) -> case PlutusTx.fromBuiltinData e of
      Nothing -> throwError "datum has wrong type"
      Just d  -> return d


dexEndpoints :: Promise (History (Either Text DexContractState)) DexSchema Void ()
dexEndpoints =
  stop
    `select` ( ( f (Proxy @"sell") historyId (const Sold) (\WithHistoryId {..} -> sell content)
                  `select` f (Proxy @"perform") historyId (const Performed) (const perform)
                  `select` f (Proxy @"findOrders") historyId Orders (const findOrders)
                  `select` f (Proxy @"funds") historyId Funds (const funds)
               )
                 <> dexEndpoints
             )
  where
    f ::
      forall l a p.
      (HasEndpoint l p DexSchema, FromJSON p) =>
      Proxy l ->
      (p -> Text) ->
      (a -> DexContractState) ->
      (p -> Contract (History (Either Text DexContractState)) DexSchema Text a) ->
      Promise (History (Either Text DexContractState)) DexSchema Void ()
    f _ getGuid g c = handleEndpoint @l $ \p -> do
      let guid = either (const "ERROR") getGuid p
      e <- either (pure . Left) (runError @_ @_ @Text . c) p

      case e of
        Left err -> do
          logInfo @Text ("Error during calling endpoint: " <> err)
          tell $ WH.append guid . Left $ err
        Right a
          | symbolVal (Proxy @l) GHC.Classes./= "clearState" ->
            tell $ WH.append guid . Right . g $ a
        _ -> return ()

    stop :: Promise (History (Either Text DexContractState)) DexSchema Void ()
    stop = handleEndpoint @"stop" $ \e -> do
      tell $ case e of
        Left err                    -> WH.append "ERROR" $ Left err
        Right (WithHistoryId hId _) -> WH.append hId $ Right Stopped
