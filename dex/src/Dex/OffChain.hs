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
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

module Dex.OffChain
  where

import           Control.Lens.Getter         (view)
import           Control.Monad               hiding (fmap, mapM, mapM_)
import           Data.Bifunctor              (bimap)
import           Data.List                   (foldl', sortOn)
import qualified Data.Map                    as Map
import           Data.Maybe                  (catMaybes)
import           Data.Proxy                  (Proxy (..))
import qualified Data.Set.Unordered.Unique   as Set
import           Data.Text                   (Text)
import           Data.UUID                   as UUID
import           Data.Void                   (Void)
import           Dex.LiquidityPool.PoolParts
import           Dex.OnChain                 (mkDexValidator)
import           Dex.Types
import           Dex.WalletHistory           as WH
import qualified GHC.Classes
import           GHC.TypeLits                (symbolVal)
import           Ledger                      hiding (fee, singleton)
import           Ledger.Constraints          (TxConstraints (..))
import qualified Ledger.Constraints          as Constraints
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                (AssetClass (..),
                                              assetClassValueOf, getValue)
import           Playground.Contract
import           Plutus.Contract
import qualified PlutusTx
import qualified PlutusTx.AssocMap           as AssocMap
import           PlutusTx.Builtins.Class     (stringToBuiltinByteString)
import           PlutusTx.Prelude            hiding (Semigroup (..), round, sum,
                                              unless, (*), (+), (-))
import           Prelude                     (Double, Semigroup (..), ceiling,
                                              fromIntegral, round, sum, (*),
                                              (/))
import qualified Prelude
import           System.Random
import           System.Random.SplitMix

type DexState = History (Either Text DexContractState)

data Dex
instance Scripts.ValidatorTypes Dex where
  type RedeemerType Dex = DexAction
  type DatumType Dex = DexDatum

dexInstance :: Scripts.TypedValidator Dex
dexInstance =
  Scripts.mkTypedValidator @Dex
    $$(PlutusTx.compile [|| mkDexValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @DexDatum @DexAction

type DexSchema =
  Endpoint "createSellOrder" (Request SellOrderParams)
  .\/ Endpoint "createLiquidityOrder" (Request LiquidityOrderParams)
  .\/ Endpoint "createLiquidityPool" (Request LiquidityPoolParams)
  .\/ Endpoint "perform" (Request ())
  .\/ Endpoint "stop" (Request ())
  .\/ Endpoint "funds" (Request ())
  .\/ Endpoint "allOrders" (Request ())
  .\/ Endpoint "myOrders" (Request ())
  .\/ Endpoint "ordersBySet" (Request AssetSet)
  .\/ Endpoint "sets" (Request ())
  .\/ Endpoint "cancel" (Request CancelOrderParams)
  .\/ Endpoint "collectFunds" (Request ())
  .\/ Endpoint "performNRandom" (Request Integer)
  .\/ Endpoint "myPayouts" (Request ())

getConstraintsForSwap :: ChainIndexTxOut -> TxOutRef  -> Order -> (TxConstraints DexAction DexDatum, Constraints.ScriptLookups Dex)
getConstraintsForSwap txOut txOutRef order@(SellOrder SellOrderInfo {..}) =
  let tx = Constraints.mustPayToTheScript
            (Payout PayoutInfo {..})
            (singleton expectedCoin expectedAmount)
        <> Constraints.mustIncludeDatum (Datum $ PlutusTx.toBuiltinData (Payout PayoutInfo {..}))
        <> Constraints.mustIncludeDatum (Datum $ PlutusTx.toBuiltinData (Order order))
        <> Constraints.mustSpendScriptOutput txOutRef (Redeemer $ PlutusTx.toBuiltinData Swap)

      lookups =
        Constraints.otherData (Datum $ PlutusTx.toBuiltinData (Payout PayoutInfo {..}))
        <> Constraints.otherData (Datum $ PlutusTx.toBuiltinData (Order order))
        <> Constraints.unspentOutputs (Map.singleton txOutRef txOut)

  in (tx,lookups)

getConstraintsForSwap txOut txOutRef order@(LiquidityOrder lo@LiquidityOrderInfo {..}) =
  let (numerator, denominator) = swapFee
      fee = fromIntegral expectedAmount Prelude.* fromIntegral numerator Prelude./ fromIntegral denominator
      integerFee = ceiling @Double @Integer fee
      newOrder = Order (LiquidityOrder (reversedLiquidityOrder (assetClassValueOf (view ciTxOutValue txOut) lockedCoin) lo))
      tx = Constraints.mustPayToTheScript
          newOrder
          (singleton expectedCoin (expectedAmount Prelude.+ Nat integerFee))
        <> Constraints.mustIncludeDatum (Datum $ PlutusTx.toBuiltinData newOrder)
        <> Constraints.mustIncludeDatum (Datum $ PlutusTx.toBuiltinData order)
        <> Constraints.mustSpendScriptOutput txOutRef (Redeemer $ PlutusTx.toBuiltinData Swap)
      lookups =
        Constraints.otherData (Datum $ PlutusTx.toBuiltinData order)
        <> Constraints.otherData (Datum $ PlutusTx.toBuiltinData order)
        <> Constraints.unspentOutputs (Map.singleton txOutRef txOut)

  in (tx,lookups)

uuidToBBS :: UUID.UUID -> BuiltinByteString
uuidToBBS = stringToBuiltinByteString . UUID.toString

createSellOrder :: SMGen -> SellOrderParams -> Contract DexState DexSchema Text UUID
createSellOrder smgen SellOrderParams {..} = do
  ownerHash <- ownPubKeyHash
  let uuid = head $ randoms @UUID.UUID smgen
  let orderInfo = SellOrderInfo
                    { expectedCoin = expectedCoin
                    , lockedCoin = lockedCoin
                    , expectedAmount = expectedAmount
                    , ownerHash = ownerHash
                    , orderId = uuidToBBS uuid
                    }
  let tx = Constraints.mustPayToTheScript (Order $ SellOrder orderInfo) (singleton lockedCoin lockedAmount)
  mkTxConstraints (Constraints.typedValidatorLookups dexInstance) tx >>= void . submitUnbalancedTx. Constraints.adjustUnbalancedTx
  return uuid

createLiquidityOrder :: SMGen -> LiquidityOrderParams -> Contract DexState DexSchema Text UUID
createLiquidityOrder smgen LiquidityOrderParams {..} = do
  ownerHash <- ownPubKeyHash
  let uuid = head $ randoms @UUID.UUID smgen
  let orderInfo =
        LiquidityOrderInfo
          { expectedCoin = expectedCoin
          , lockedCoin = lockedCoin
          , expectedAmount = expectedAmount
          , swapFee = swapFee
          , ownerHash = ownerHash
          , orderId = uuidToBBS uuid
          }
  let tx = Constraints.mustPayToTheScript (Order $ LiquidityOrder orderInfo) (singleton lockedCoin lockedAmount)
  mkTxConstraints (Constraints.typedValidatorLookups dexInstance) tx >>= void . submitUnbalancedTx . Constraints.adjustUnbalancedTx
  return uuid

createLiquidityPool :: SMGen -> LiquidityPoolParams -> Contract DexState DexSchema Text ()
createLiquidityPool smgen LiquidityPoolParams {..} = do
  NormalizedParts {..} <- case generateNormalizedParts poolPartsParams of
    Right p   -> logInfo @Prelude.String (Prelude.show p) >> return p
    Left  err -> throwError err

  let partsA@(coreA:_) = map (* (fromIntegral amountA / sum normalizedPartsA)) normalizedPartsA
  let virtualPartsA = map (* (fromIntegral amountA / sum normalizedPartsA)) normalizedVirtualPartsA

  let partsB@(coreB:_) = map (* ((coreA / toDouble exchangeRate) / head normalizedPartsB)) normalizedPartsB
  let virtualPartsB = map (* ((coreA / toDouble exchangeRate) / head normalizedPartsB)) normalizedVirtualPartsB

  let lpPartsA = zipWith (curry $ bimap round round) partsA (coreB:virtualPartsB)
  let lpPartsB = zipWith (curry $ bimap round round) partsB (coreA:virtualPartsA)
  ownerHash <- ownPubKeyHash
  let (smgenA, smgenB) = splitSMGen smgen

  let uuidsA = randoms @UUID.UUID smgenA
  let liquidityOrdersA = flip map (zip uuidsA lpPartsA)
        $ \(uuid, (a,b)) -> ( (a, coinA)
                            , LiquidityOrderInfo
                              { expectedCoin   = coinB
                              , lockedCoin     = coinA
                              , expectedAmount = Nat b
                              , swapFee        = swapFee
                              , ownerHash      = ownerHash
                              , orderId        = uuidToBBS uuid
                              }
                            )
  let uuidsB = randoms @UUID.UUID smgenB
  let liquidityOrdersB = flip map (zip uuidsB lpPartsB)
        $ \(uuid, (b,a)) -> ( (b, coinB)
                            , LiquidityOrderInfo
                              { expectedCoin   = coinA
                              , lockedCoin     = coinB
                              , expectedAmount = Nat a
                              , swapFee        = swapFee
                              , ownerHash      = ownerHash
                              , orderId        = uuidToBBS uuid
                              }
                            )
  let txs = flip map (liquidityOrdersA ++ liquidityOrdersB)
            $ \((amount, coin), order) -> Constraints.mustPayToTheScript (Order $ LiquidityOrder order) (singleton coin amount)
  mkTxConstraints (Constraints.typedValidatorLookups dexInstance) (mconcat txs) >>= void . submitUnbalancedTx . Constraints.adjustUnbalancedTx


perform :: Contract DexState DexSchema Text ()
perform = do
  pkh <- ownPubKeyHash
  let address = Ledger.scriptAddress $ Scripts.validatorScript dexInstance
  utxos <- Map.toList <$> utxosAt address
  mapped <- mapM (\(oref, txOut) -> getDexDatum txOut >>= \d -> return (txOut, oref, d)) utxos
  let filtered = [(txOut, oref, o) | (txOut, oref, Order o) <- mapped]

  when (PlutusTx.Prelude.null filtered) $
    throwError "No orders selected"
  let (orderTx, orderLookups) = foldl' (\(tx',lookups') (o, oref, order) -> let (tx'', lookups'') = getConstraintsForSwap o oref order in (tx' Prelude.<> tx'', lookups' Prelude.<> lookups'')) (Prelude.mempty, Prelude.mempty) filtered

  let lookups = Constraints.typedValidatorLookups dexInstance
          <> Constraints.ownPubKeyHash pkh
          <> Constraints.otherScript (Scripts.validatorScript dexInstance)
          <> orderLookups
      tx = Constraints.mustBeSignedBy pkh
        <> orderTx
  mkTxConstraints lookups tx >>= \u -> do
    let au = Constraints.adjustUnbalancedTx u
    void $ submitUnbalancedTx au

-- function for testing only
performNRandom :: SMGen -> Integer -> Contract DexState DexSchema Text ()
performNRandom smgen n = do
  pkh <- ownPubKeyHash
  let address = Ledger.scriptAddress $ Scripts.validatorScript dexInstance
  utxos <- Map.toList <$> utxosAt address
  mapped <- mapM (\(oref, txOut) -> getDexDatum txOut >>= \d -> return (txOut, oref, d)) utxos
  let filtered = [(txOut, oref, o) | (txOut, oref, Order o) <- mapped]
  let sorted = flip sortOn filtered $ \(_,_,o) ->
                  case o of
                    LiquidityOrder LiquidityOrderInfo {..} -> orderId
                    SellOrder SellOrderInfo {..}           -> orderId
  let indices = take n $ map (`Prelude.mod` length sorted) $ nub $ randoms @Integer smgen
  let selected = map (sorted!!) indices

  when (PlutusTx.Prelude.null selected) $
    throwError "No orders selected"

  let (orderTx, orderLookups) = foldl' (\(tx',lookups') (o, oref, order) ->
        let (tx'', lookups'') = getConstraintsForSwap o oref order
        in (tx' Prelude.<> tx'', lookups' Prelude.<> lookups'')) (Prelude.mempty, Prelude.mempty) selected

  let lookups = Constraints.typedValidatorLookups dexInstance
          <> Constraints.ownPubKeyHash pkh
          <> Constraints.otherScript (Scripts.validatorScript dexInstance)
          <> orderLookups
      tx = Constraints.mustBeSignedBy pkh
        <> orderTx
  mkTxConstraints lookups tx >>= \u -> do
    let au = Constraints.adjustUnbalancedTx u
    void $ submitUnbalancedTx au


funds :: Contract w s Text [(AssetClass, Integer)]
funds = do
  pkh <- ownPubKeyHash
  os <- Map.elems <$> utxosAt (pubKeyHashAddress pkh)
  let walletValue = getValue $ mconcat [view ciTxOutValue o | o <- os]
  return [(AssetClass (cs, tn),  a) | (cs, tns) <- AssocMap.toList walletValue, (tn, a) <- AssocMap.toList tns]

myOrders :: Contract DexState DexSchema Text [OrderInfo]
myOrders = do
  pkh <- ownPubKeyHash
  let address = Ledger.scriptAddress $ Scripts.validatorScript dexInstance
  utxos <- Map.toList <$> utxosAt address
  mapped <- catMaybes <$> mapM toOrderInfo utxos
  return $ filter (\OrderInfo {..} -> ownerHash == pkh) mapped

allOrders :: Contract DexState DexSchema Text [OrderInfo]
allOrders = do
  let address = Ledger.scriptAddress $ Scripts.validatorScript dexInstance
  utxos <- Map.toList <$> utxosAt address
  catMaybes <$> mapM toOrderInfo utxos

ordersBySet :: AssetSet -> Contract DexState DexSchema Text [OrderInfo]
ordersBySet (AssetSet lc ec)= do
  let address = Ledger.scriptAddress $ Scripts.validatorScript dexInstance
  utxos <- Map.toList <$> utxosAt address
  mapped <- catMaybes <$> mapM toOrderInfo utxos
  return $ filter (\OrderInfo {..} -> lockedCoin == lc && expectedCoin == ec) mapped

sets :: Contract DexState DexSchema Text [AssetSet]
sets = do
  let address = Ledger.scriptAddress $ Scripts.validatorScript dexInstance
  utxos <- Map.toList <$> utxosAt address
  orders <- catMaybes <$> mapM toOrderInfo utxos
  return $ Set.unUUSet $ foldl' (\acc OrderInfo {..} -> let temp = AssetSet lockedCoin expectedCoin in Set.insert temp acc) Set.empty orders

toOrderInfo :: (TxOutRef, ChainIndexTxOut) -> Contract w s Text (Maybe OrderInfo)
toOrderInfo (orderHash, o) = do
  datum <- getDexDatum o
  case datum of
    Payout _ -> return Nothing
    Order order ->
      case order of
        LiquidityOrder LiquidityOrderInfo {..} ->
          let orderType = "Liquidity"
              lockedAmount = Nat (assetClassValueOf (view ciTxOutValue o) lockedCoin)
          in return $ Just OrderInfo {..}
        SellOrder SellOrderInfo {..} ->
          let orderType = "Sell"
              lockedAmount = Nat (assetClassValueOf (view ciTxOutValue o) lockedCoin)
          in return $ Just OrderInfo {..}

cancel :: CancelOrderParams -> Contract DexState DexSchema Text ()
cancel CancelOrderParams {..} = do
  pkh <- ownPubKeyHash
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

  mkTxConstraints lookups tx >>= void . submitUnbalancedTx . Constraints.adjustUnbalancedTx

  where
    toOwnerHash :: ChainIndexTxOut -> Contract w s Text PubKeyHash
    toOwnerHash o = do
      order <- getOrderDatum o
      case order of
        SellOrder SellOrderInfo {..}           -> return ownerHash
        LiquidityOrder LiquidityOrderInfo {..} -> return ownerHash

collectFunds :: Contract DexState DexSchema Text ()
collectFunds = do
  pkh <- ownPubKeyHash
  let address = Ledger.scriptAddress $ Scripts.validatorScript dexInstance
  utxos <- Map.toList <$> utxosAt address
  payouts <- catMaybes <$> mapM toPayoutInfo utxos
  let ownedPayouts = filter (\(_, PayoutInfo{..}) -> ownerHash == pkh) payouts
  let lookups =
        Constraints.typedValidatorLookups dexInstance
        <> Constraints.ownPubKeyHash pkh
        <> Constraints.otherScript (Scripts.validatorScript dexInstance)
        <> Constraints.unspentOutputs (Map.fromList $ filter (\(txOutRef, _) -> any ((txOutRef ==) . fst) ownedPayouts) utxos)
  let tx = mconcat $ map (\(txOutRef, _) ->
                            Constraints.mustSpendScriptOutput txOutRef $ Redeemer $ PlutusTx.toBuiltinData CollectCoins) ownedPayouts
  mkTxConstraints lookups tx >>= void . submitUnbalancedTx . Constraints.adjustUnbalancedTx
  where
  toPayoutInfo (txOutRef, txOut) = do
    datum <- getDexDatum txOut
    case datum of
      Order _           -> return Nothing
      Payout payoutInfo -> return $ Just (txOutRef, payoutInfo)

myPayouts :: Contract DexState DexSchema Text PayoutSummary
myPayouts = do
  pkh <- ownPubKeyHash
  let address = Ledger.scriptAddress $ Scripts.validatorScript dexInstance
  utxos <- Map.toList <$> utxosAt address
  mapped <- catMaybes <$> mapM toPayoutInfo utxos
  let filtered = filter (\(_,PayoutInfo {..}) -> ownerHash == pkh) mapped
  let totalValue = valueToList $ mconcat $ map fst filtered
  return $ PayoutSummary {payoutValue=totalValue}
  where

    valueToList :: Value -> [(AssetClass, Integer)]
    valueToList = concatMap (\(k,v) -> map (\(k',v') -> (AssetClass (k,k'), v')) $ AssocMap.toList v) . AssocMap.toList . getValue

    toPayoutInfo (_, o) = do
      datum <- getDexDatum o
      case datum of
        Payout p -> return $ Just (view ciTxOutValue o,p)
        _        -> return Nothing

getDexDatum :: ChainIndexTxOut -> Contract w s Text DexDatum
getDexDatum ScriptChainIndexTxOut { _ciTxOutDatum } = do
        (Datum e) <- either getDatum pure _ciTxOutDatum
        maybe (throwError "datum hash wrong type") pure (PlutusTx.fromBuiltinData e)
  where
    getDatum :: DatumHash -> Contract w s Text Datum
    getDatum =
      datumFromHash >=>
      \case Nothing -> throwError "datum not found"
            Just d  -> pure d
getDexDatum _ = throwError "no datum for a txout of a public key address"

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

dexEndpoints :: Contract DexState DexSchema Void ()
dexEndpoints =
  selectList
  [ stop'
  , createSellOrder'
  , createLiquidityOrder'
  , createLiquidityPool'
  , perform'
  , myOrders'
  , allOrders'
  , ordersBySet'
  , sets'
  , funds'
  , cancel'
  , collectFunds'
  , performNRandom'
  , myPayouts'
  ] >> dexEndpoints
  where
    f ::
      forall l a p.
      (HasEndpoint l p DexSchema, FromJSON p) =>
      Proxy l ->
      (p -> Text) ->
      (a -> DexContractState) ->
      (p -> Contract DexState DexSchema Text a) ->
      Promise DexState DexSchema Void ()
    f _ getHistoryId g c = handleEndpoint @l $ \p -> do
      let hid = either (const "ERROR") getHistoryId p
      e <- either (pure . Left) (runError @_ @_ @Text . c) p

      case e of
        Left err -> do
          logInfo @Text ("Error during calling endpoint: " <> err)
          tell $ WH.append hid . Left $ err
        Right a
          | symbolVal (Proxy @l) GHC.Classes./= "clearState" ->
            tell $ WH.append hid . Right . g $ a
        _ -> return ()

    stop' :: Promise DexState DexSchema Void ()
    stop' = handleEndpoint @"stop" $ \e -> do
      tell $ case e of
        Left err                -> WH.append "ERROR" $ Left err
        Right (Request hId _ _) -> WH.append hId $ Right Stopped

    createLiquidityOrder' = f (Proxy @"createLiquidityOrder") historyId (const OrderCreated) (\Request {..} -> createLiquidityOrder (mkSMGen $ fromIntegral randomSeed) content)
    createSellOrder'      = f (Proxy @"createSellOrder") historyId (const OrderCreated) (\Request {..} -> createSellOrder (mkSMGen $ fromIntegral randomSeed) content)
    createLiquidityPool'  = f (Proxy @"createLiquidityPool") historyId (const PoolCreated) (\Request {..} -> createLiquidityPool (mkSMGen $ fromIntegral randomSeed) content)
    perform'              = f (Proxy @"perform") historyId (const Performed) (const perform)
    myOrders'             = f (Proxy @"myOrders") historyId MyOrders (const myOrders)
    allOrders'            = f (Proxy @"allOrders") historyId AllOrders (const allOrders)
    ordersBySet'          = f (Proxy @"ordersBySet") historyId OrdersBySet (\Request {..} -> ordersBySet content)
    sets'                 = f (Proxy @"sets") historyId Sets (const sets)
    funds'                = f (Proxy @"funds") historyId Funds (const funds)
    cancel'               = f (Proxy @"cancel") historyId (const Canceled) (\Request {..} -> cancel content)
    collectFunds'         = f (Proxy @"collectFunds") historyId (const Collected) (const collectFunds)
    performNRandom'       = f (Proxy @"performNRandom") historyId (const Performed) (\Request {..} -> performNRandom (mkSMGen $ fromIntegral randomSeed) content)
    myPayouts'            = f (Proxy @"myPayouts") historyId MyPayouts (const myPayouts)
