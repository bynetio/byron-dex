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

import           Control.Lens.Getter     (view)
import           Control.Monad           hiding (fmap, mapM, mapM_)
import           Data.List               (foldl')
import qualified Data.Map                as Map
import           Data.Proxy              (Proxy (..))
import           Data.Text               (Text)
import           Data.UUID               as UUID
import           Data.Void               (Void)
import           Dex.OnChain             (mkDexValidator)
import           Dex.Types
import           Dex.WalletHistory       as WH
import qualified GHC.Classes
import           GHC.TypeLits            (symbolVal)
import           Ledger                  hiding (fee, singleton)
import           Ledger.Constraints      as Constraints
import qualified Ledger.Typed.Scripts    as Scripts
import           Ledger.Value            (AssetClass (..), assetClassValue,
                                          assetClassValueOf, getValue)
import           Playground.Contract
import           Plutus.Contract
import qualified PlutusTx
import qualified PlutusTx.AssocMap       as AssocMap
import           PlutusTx.Builtins.Class (stringToBuiltinByteString)
import           PlutusTx.Prelude        hiding (Semigroup (..), unless)
import           Prelude                 (Double, Semigroup (..), ceiling,
                                          fromIntegral, (/))
import qualified Prelude
import           System.Random
import           System.Random.SplitMix
data Dex

instance Scripts.ValidatorTypes Dex where
  type RedeemerType Dex = DexAction
  type DatumType Dex = DexDatum


dexInstance :: Scripts.TypedValidator Dex
dexInstance =
  Scripts.mkTypedValidator @Dex
    $$(PlutusTx.compile [||mkDexValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @DexDatum @DexAction


type DexSchema =
  Endpoint "sell" (Request SellOrderParams)
  .\/ Endpoint "perform" (Request ())
  .\/ Endpoint "stop" (Request ())
  .\/ Endpoint "funds" (Request ())
  .\/ Endpoint "findOrders" (Request ())
  .\/ Endpoint "orders" (Request ())
  .\/ Endpoint "cancel" (Request CancelOrderParams)



getConstraintsForSwap :: ChainIndexTxOut -> TxOutRef  -> Order -> TxConstraints DexAction DexDatum
getConstraintsForSwap _ txOutRef (SellOrder SellOrderInfo {..}) =
  Constraints.mustPayToTheScript
    (Payout PayoutInfo {..})
    (assetClassValue expectedCoin (fromNat expectedAmount))
  <> Constraints.mustSpendScriptOutput txOutRef (Redeemer $ PlutusTx.toBuiltinData Swap)

getConstraintsForSwap txOut txOutRef (LiquidityOrder lo@LiquidityOrderInfo {..}) =
  let (numerator, denominator) = swapFee
  in Constraints.mustPayToTheScript
    (Payout PayoutInfo {..})
    (assetClassValue expectedCoin (ceiling @Double @Integer (fromIntegral expectedAmount Prelude.* fromIntegral numerator Prelude./ fromIntegral denominator)))
  <> Constraints.mustPayToTheScript
    (Order (LiquidityOrder (reversedLiquidityOrder (assetClassValueOf (view ciTxOutValue txOut) lockedCoin) lo)))
    (assetClassValue expectedCoin (fromNat expectedAmount))
  <> Constraints.mustSpendScriptOutput txOutRef (Redeemer $ PlutusTx.toBuiltinData Swap)



uuidToBBS :: UUID.UUID -> BuiltinByteString
uuidToBBS = stringToBuiltinByteString . UUID.toString

sell :: SMGen -> SellOrderParams -> Contract (History (Either Text DexContractState)) DexSchema Text ()
sell smgen SellOrderParams {..} = do
  ownerHash <- pubKeyHash <$> ownPubKey
  let uuid = head $ randoms @UUID.UUID smgen
  let orderInfo = SellOrderInfo
                    { expectedCoin = expectedCoin
                    , lockedCoin = lockedCoin
                    , expectedAmount = expectedAmount
                    , ownerHash = ownerHash
                    , orderId = uuidToBBS uuid
                    }
  let tx = Constraints.mustPayToTheScript (Order $ SellOrder orderInfo) (assetClassValue lockedCoin (fromNat lockedAmount))
  void $ submitTxConstraints dexInstance tx



-- buy :: SellOrderParams -> Contract (History (Either Text DexContractState)) DexSchema Text ()
-- buy SellOrderParams {..} = do
--   ownerHash <- pubKeyHash <$> ownPubKey

--   let address = Ledger.scriptAddress $ Scripts.validatorScript dexInstance
--   utxos <- Map.toList <$> utxosAt address
--   mapped <- mapM (\(oref, o) -> getOrder o >>= \d -> return (o, oref, d)) utxos
--   let filtered =
--         [ (oref, o, d)
--         | (oref, o, d@(SellOrder (SellOrderInfo coinIn' coinOut' _ _))) <- mapped
--         , coinIn == coinOut'
--         , coinOut == coinIn'
--         ]
--   let sorted = sortOn (\(o,_,SellOrder SellOrderInfo {coinIn=coinIn', expectedAmount=expectedAmount'}) ->
--                                   let amountIn' = assetClassValueOf (view ciTxOutValue o) coinIn'
--                                   in fromIntegral (fromNat expectedAmount') / fromIntegral amountIn') filtered


--   let (utxosToCollect, _, reward) =
--         foldl' (\(utxosAcc, moneyToSpend, rewardAcc) order@(o,oref,SellOrder (SellOrderInfo coinIn' coinOut' cost _)) ->
--           let reward = assetClassValueOf (view ciTxOutValue o) coinIn'
--           in if moneyToSpend - fromNat cost < 0
--             then (utxosAcc, moneyToSpend, rewardAcc)
--             else (order:utxosAcc, moneyToSpend - fromNat cost, rewardAcc + reward)
--           ) ([], fromNat amountIn, 0) sorted
--   when (reward < fromNat expectedAmount) throwError "couldn't find suiting orders"

--   let tx = foldl' (\acc (o, oref, SellOrder SellOrderInfo {..}) ->
--               let inValue = assetClassValueOf (view ciTxOutValue o) coinIn

--               in acc <> Constraints.mustPayToPubKey ownerHash (assetClassValue coinOut (fromNat expectedAmount))
--                     <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Perform)
--               ) mempty utxosToCollect
--   let lookups = Constraints.typedValidatorLookups dexInstance
--         <> Constraints.ownPubKeyHash ownerHash
--         <> Constraints.otherScript (Scripts.validatorScript dexInstance)
--         <> Constraints.unspentOutputs (Map.fromList (map (\(o,oref,_) -> (oref,o)) utxosToCollect))
--   void $ submitTxConstraintsWith lookups tx



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
      tx = foldl' (\acc (o, oref, order) -> acc <> getConstraintsForSwap o oref order
        ) mempty mapped
  void $ submitTxConstraintsWith lookups tx


funds :: Contract w s Text [(AssetClass, Integer)]
funds = do
  pkh <- pubKeyHash <$> ownPubKey
  os <- Map.elems <$> utxosAt (pubKeyHashAddress pkh)
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
      order <- getOrderDatum o
      case order of
        LiquidityOrder LiquidityOrderInfo {..} ->
          let orderType = "Liquidity"
              lockedAmount = Nat (assetClassValueOf (view ciTxOutValue o) lockedCoin)
          in return OrderInfo {..}
        SellOrder      SellOrderInfo      {..} ->
          let orderType = "Sell"
              lockedAmount = Nat (assetClassValueOf (view ciTxOutValue o) lockedCoin)
          in return OrderInfo {..}

cancel :: CancelOrderParams -> Contract (History (Either Text DexContractState)) DexSchema Text ()
cancel CancelOrderParams {..} = do
  pkh <- pubKeyHash <$> ownPubKey
  let address = Ledger.scriptAddress $ Scripts.validatorScript dexInstance
  utxos  <- Map.toList . Map.filterWithKey (\oref' _ -> oref' == orderHash) <$> utxosAt address
  hashes <- mapM (toOwnerHash . snd) utxos

  when (any (/= pkh) hashes || Prelude.null hashes) (throwError "Cannot find order by provided hash")

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
      order <- getOrderDatum o
      case order of
        SellOrder SellOrderInfo {..}           -> return ownerHash
        LiquidityOrder LiquidityOrderInfo {..} -> return ownerHash


getOrderDatum :: ChainIndexTxOut -> Contract w s Text Order
getOrderDatum ScriptChainIndexTxOut { _ciTxOutDatum } = do
        (Datum e) <- either getDatum pure _ciTxOutDatum
        o <- maybe (throwError "datum hash wrong type")
              pure
              (PlutusTx.fromBuiltinData e)
        case o of
          Order order -> return order
          _           -> throwError "datum hash wrong type"
  where
    getDatum :: DatumHash -> Contract w s Text Datum
    getDatum =
      datumFromHash >=>
      \case Nothing -> throwError "datum not found"
            Just d  -> pure d
getOrderDatum _ = throwError "no datum for a txout of a public key address"


dexEndpoints :: Promise (History (Either Text DexContractState)) DexSchema Void ()
dexEndpoints =
  stop
    `select` ( ( f (Proxy @"sell") historyId (const Sold) (\Request {..} -> sell (mkSMGen $ fromIntegral randomSeed) content)
                  `select` f (Proxy @"perform") historyId (const Performed) (const perform)
                  `select` f (Proxy @"findOrders") historyId Orders (const findOrders)
                  `select` f (Proxy @"orders") historyId MyOrders (const orders)
                  `select` f (Proxy @"funds") historyId Funds (const funds)
                  `select` f (Proxy @"cancel") historyId (const Cancel) (\Request {..} -> cancel content)
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
        Left err                -> WH.append "ERROR" $ Left err
        Right (Request hId _ _) -> WH.append hId $ Right Stopped
