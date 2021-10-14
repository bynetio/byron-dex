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

{-# LANGUAGE NamedFieldPuns             #-}
module Dex.OffChain
  where

import           Control.Lens.Getter  (view)
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
import           Ledger.Value         (AssetClass (..), assetClassValue, assetClassValueOf, getValue)
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
    $$(PlutusTx.compile [||mkDexValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @DexDatum @DexAction


type DexSchema =
  Endpoint "sell" (WithHistoryId SellOrderParams)
  .\/ Endpoint "perform" (WithHistoryId ())
  .\/ Endpoint "stop" (WithHistoryId ())
  .\/ Endpoint "funds" (WithHistoryId ())
  .\/ Endpoint "findOrders" (WithHistoryId ())
  .\/ Endpoint "orders" (WithHistoryId ())
  .\/ Endpoint "cancel" (WithHistoryId CancelOrderParams)


sell :: SellOrderParams -> Contract (History (Either Text DexContractState)) DexSchema Text ()
sell SellOrderParams {..} = do
  ownerHash <- pubKeyHash <$> ownPubKey

  let address = Ledger.scriptAddress $ Scripts.validatorScript dexInstance
  utxos <- Map.toList <$> utxosAt address
  mapped <- mapM (\(oref, o) -> getOrderDatum o >>= \d -> return (o, oref, d)) utxos
  let filtered =
        [ (oref, o, d)
        | (oref, o, d@(SellOrder (SellOrderInfo coinIn' coinOut' _ _))) <- mapped
        , coinIn == coinOut'
        , coinOut == coinIn'
        ]
  let sorted = sortOn (\(o,_,SellOrder SellOrderInfo {coinIn=coinIn', expectedAmount=expectedAmount'}) ->
                                  let amountIn' = assetClassValueOf (view ciTxOutValue o) coinIn'
                                  in fromIntegral (fromNat expectedAmount') / fromIntegral amountIn') filtered


  let (utxosToCollect, _, reward) =
        foldl' (\(utxosAcc, moneyToSpend, rewardAcc) order@(o,oref,SellOrder (SellOrderInfo coinIn' coinOut' cost _)) ->
          let reward = assetClassValueOf (view ciTxOutValue o) coinIn'
          in if moneyToSpend - fromNat cost < 0
            then (utxosAcc, moneyToSpend, rewardAcc)
            else (order:utxosAcc, moneyToSpend - fromNat cost, rewardAcc + reward)
          ) ([], fromNat amountIn, 0) sorted
  if reward >= fromNat expectedAmount
    then do
      let tx = foldl' (\acc (o, oref, SellOrder SellOrderInfo {..}) ->
                let inValue = assetClassValueOf (view ciTxOutValue o) coinIn

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
  utxos <- Map.toList <$> utxosAt address
  mapM (\(oref, o) -> getOrderDatum o >>= \case
    SellOrder sellOrder -> return (sellOrder, oref)
    ) utxos


perform :: Contract (History (Either Text DexContractState)) DexSchema Text ()
perform = do
  pkh <- pubKeyHash <$> ownPubKey
  let address = Ledger.scriptAddress $ Scripts.validatorScript dexInstance
  utxos <- Map.toList <$> utxosAt address
  mapped <- mapM (\(oref, o) -> getOrderDatum o >>= \d -> return (o, oref, d)) utxos
  let lookups = Constraints.typedValidatorLookups dexInstance
          <> Constraints.ownPubKeyHash pkh
          <> Constraints.otherScript (Scripts.validatorScript dexInstance)
          <> Constraints.unspentOutputs (Map.fromList utxos)
      tx = foldl' (\acc (o, oref, SellOrder SellOrderInfo {..}) ->
        let inValue = assetClassValueOf (view ciTxOutValue o) coinIn

        in acc <> Constraints.mustPayToPubKey ownerHash (assetClassValue coinOut (fromNat expectedAmount))
              <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Perform)
        ) mempty mapped
  void $ submitTxConstraintsWith lookups tx


funds :: Contract w s Text [(AssetClass, Integer)]
funds = do
  pkh <- pubKeyHash <$> ownPubKey
  os <- map snd . Map.toList <$> utxosAt (pubKeyHashAddress pkh)
  let walletValue = getValue $ mconcat [view ciTxOutValue o | o <- os]
  return [(AssetClass (cs, tn),  a) | (cs, tns) <- AssocMap.toList walletValue, (tn, a) <- AssocMap.toList tns]


orders :: Contract (History (Either Text DexContractState)) DexSchema Text [OrderInfo]
orders = do
  pkh <- pubKeyHash <$> ownPubKey
  let address = Ledger.scriptAddress $ Scripts.validatorScript dexInstance
  utxos <- Map.toList <$> utxosAt address
  mapped <- mapM toOrderInfo utxos
  return $ filter (\OrderInfo {..} -> ownerHash == pkh) mapped
  where
    toOrderInfo (orderHash, o) = do
      SellOrder SellOrderInfo {..} <- getOrderDatum o
      return $ OrderInfo {..}

cancel :: CancelOrderParams -> Contract (History (Either Text DexContractState)) DexSchema Text ()
cancel CancelOrderParams {..} = do
  pkh <- pubKeyHash <$> ownPubKey
  let address = Ledger.scriptAddress $ Scripts.validatorScript dexInstance
  utxos  <- Map.toList . Map.filterWithKey (\oref' _ -> oref' == orderHash) <$> utxosAt address
  hashes <- mapM (toOwnerHash . snd) utxos

  when (any (/= pkh) hashes || null hashes) (throwError "Cannot find order by provided hash")

  let lookups =
        Constraints.typedValidatorLookups dexInstance
        <> Constraints.ownPubKeyHash pkh
        <> Constraints.otherScript (Scripts.validatorScript dexInstance)
        <> Constraints.unspentOutputs (Map.fromList utxos)

      tx     = Constraints.mustSpendScriptOutput orderHash $ Redeemer $ PlutusTx.toBuiltinData CancelOrder

  void $ submitTxConstraintsWith lookups tx

  where
    toOwnerHash :: ChainIndexTxOut -> Contract w s Text PubKeyHash
    toOwnerHash o = do
      SellOrder SellOrderInfo {..} <- getOrderDatum o
      return ownerHash


getOrderDatum :: ChainIndexTxOut -> Contract w s Text DexDatum
getOrderDatum o =
  case o of
      PublicKeyChainIndexTxOut {} ->
        throwError "no datum for a txout of a public key address"
      ScriptChainIndexTxOut { _ciTxOutDatum } -> do
        (Datum e) <- either getDatum pure _ciTxOutDatum
        maybe (throwError "datum hash wrong type")
              pure
              (PlutusTx.fromBuiltinData e)
  where
    getDatum :: DatumHash -> Contract w s Text Datum
    getDatum dh =
      datumFromHash dh >>= \case Nothing -> throwError "datum not found"
                                 Just d  -> pure d


dexEndpoints :: Promise (History (Either Text DexContractState)) DexSchema Void ()
dexEndpoints =
  stop
    `select` ( ( f (Proxy @"sell") historyId (const Sold) (\WithHistoryId {..} -> sell content)
                  `select` f (Proxy @"perform") historyId (const Performed) (const perform)
                  `select` f (Proxy @"findOrders") historyId Orders (const findOrders)
                  `select` f (Proxy @"orders") historyId MyOrders (const orders)
                  `select` f (Proxy @"funds") historyId Funds (const funds)
                  `select` f (Proxy @"cancel") historyId (const Cancel) (\WithHistoryId {..} -> cancel content)
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
