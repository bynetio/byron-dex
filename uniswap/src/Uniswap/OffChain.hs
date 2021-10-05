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
module Uniswap.OffChain
  ( poolStateCoinFromUniswcurrency,
    liquidityCoin,
    CreateParams (..),
    SwapParams (..),
    SwapPreviewParams (..),
    IndirectSwapParams (..),
    ISwapPreviewParams (..),
    CloseParams (..),
    RemoveParams (..),
    AddParams (..),
    ClearStateParams (..),
    UniswapUserSchema,
    UserContractState (..),
    UniswapOwnerSchema,
    UniswapOwnerSchema',
    start,
    create,
    add,
    remove,
    close,
    swap,
    pools,
    ownerEndpoint,
    ownerEndpoint',
    userEndpoints,
    uniswap,
    uniswapTokenName,
    WithHistoryId (..),
  )
where

import           Control.Applicative          ((<|>))
import           Control.Lens                 (view)
import           Control.Monad                hiding (fmap, mapM, mapM_)
import           Control.Monad.Freer
import           Data.List                    (scanl)
import qualified Data.Map                     as Map
import           Data.Proxy                   (Proxy (..))
import           Data.Text                    (Text, pack)
import           Data.Void                    (Void)
import qualified GHC.Classes
import           GHC.TypeLits                 (symbolVal)
import           Ledger                       hiding (fee, singleton)
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value                 (getValue)
import           Playground.Contract
import           Plutus.Contract
import qualified Plutus.Contracts.Currency    as Currency
import qualified PlutusTx
import qualified PlutusTx.AssocMap            as AssocMap
import           PlutusTx.Prelude             hiding (Semigroup (..), unless)
import           Prelude                      (Semigroup (..), String, div, show)
import           Text.Printf                  (printf)
import           Uniswap.Common.WalletHistory (History)
import qualified Uniswap.Common.WalletHistory as WH
import           Uniswap.IndirectSwaps
import           Uniswap.OnChain              (mkUniswapValidator, validateLiquidityMinting)
import           Uniswap.Pool
import           Uniswap.Types                hiding (liquidityCoin)

data Uniswapping

instance Scripts.ValidatorTypes Uniswapping where
  type RedeemerType Uniswapping = UniswapAction
  type DatumType Uniswapping = UniswapDatum

type UniswapOwnerSchema =
  Endpoint "start" (WithHistoryId ())

type UniswapOwnerSchema' =
  Endpoint "start" (WithHistoryId CurrencySymbol)

-- | Schema for the endpoints for users of Uniswap.
type UniswapUserSchema =
  Endpoint "create" (WithHistoryId CreateParams)
    .\/ Endpoint "swap" (WithHistoryId SwapParams)
    .\/ Endpoint "swapPreview" (WithHistoryId SwapPreviewParams)
    .\/ Endpoint "iSwapPreview" (WithHistoryId ISwapPreviewParams)
    .\/ Endpoint "iSwap" (WithHistoryId IndirectSwapParams)
    .\/ Endpoint "close" (WithHistoryId CloseParams)
    .\/ Endpoint "remove" (WithHistoryId RemoveParams)
    .\/ Endpoint "add" (WithHistoryId AddParams)
    .\/ Endpoint "pools" (WithHistoryId ())
    .\/ Endpoint "funds" (WithHistoryId ())
    .\/ Endpoint "stop" (WithHistoryId ())
    .\/ Endpoint "clearState" (WithHistoryId ClearStateParams)

-- | Type of the Uniswap user contract state.
data UserContractState
  = AvailablePools [LiquidityPoolView]
  | AvailableFunds [AmountOfCoin A]
  | Created
  | Swapped
  | SwapPreviewResult SwapPreviewResultData
  | ISwapped
  | ISwapPreviewResult ISwapPreviewResultData
  | Added
  | Removed
  | Closed
  | Stopped
  | Cleared
  deriving (Show, Generic, FromJSON, ToJSON)

uniswapTokenName, poolStateTokenName :: TokenName
uniswapTokenName = "Uniswap"
poolStateTokenName = "Pool State"

uniswapInstance :: Uniswap -> Scripts.TypedValidator Uniswapping
uniswapInstance us =
  Scripts.mkTypedValidator @Uniswapping
    ( $$(PlutusTx.compile [||mkUniswapValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode us
        `PlutusTx.applyCode` PlutusTx.liftCode c
    )
    $$(PlutusTx.compile [||wrap||])
  where
    c :: Coin PoolState
    c = poolStateCoin us

    wrap = Scripts.wrapValidator @UniswapDatum @UniswapAction

uniswapScript :: Uniswap -> Validator
uniswapScript = Scripts.validatorScript . uniswapInstance

uniswapAddress :: Uniswap -> Ledger.Address
uniswapAddress = Ledger.scriptAddress . uniswapScript

-- | Given a currency symbol creates NFT token to identify a uniswap instance
uniswap :: CurrencySymbol -> Uniswap
uniswap cs = Uniswap $ mkCoin cs uniswapTokenName

liquidityPolicy :: Uniswap -> MintingPolicy
liquidityPolicy us =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||\u t -> Scripts.wrapMintingPolicy (validateLiquidityMinting u t)||])
      `PlutusTx.applyCode` PlutusTx.liftCode us
      `PlutusTx.applyCode` PlutusTx.liftCode poolStateTokenName

liquidityCurrency :: Uniswap -> CurrencySymbol
liquidityCurrency = scriptCurrencySymbol . liquidityPolicy

poolStateCoin :: Uniswap -> Coin PoolState
poolStateCoin = flip mkCoin poolStateTokenName . liquidityCurrency

-- | Gets the 'Coin' used to identity liquidity pools.
poolStateCoinFromUniswcurrency ::
  -- | The currency identifying the Uniswap instance.
  CurrencySymbol ->
  Coin PoolState
poolStateCoinFromUniswcurrency = poolStateCoin . uniswap

-- | Gets the liquidity token for a given liquidity pool.
liquidityCoin ::
  -- | The currency identifying the Uniswap instance.
  CurrencySymbol ->
  -- | One coin in the liquidity pair.
  Coin A ->
  -- | The other coin in the liquidity pair.
  Coin B ->
  -- | Numerator and denominator of the swap fee
  Fee ->
  Coin Liquidity
liquidityCoin cs coinA coinB fee = mkCoin (liquidityCurrency $ uniswap cs) $ lpTicker $ liquidityPool (coinA, coinB) fee

-- | Creates a Uniswap "factory". This factory will keep track of the existing liquidity pools and enforce that there will be at most one liquidity pool
-- for any pair of tokens at any given time.
start ::
  -- | for test purpose we can pass our currency symbol
  Maybe CurrencySymbol ->
  Contract w s Text Uniswap
start mcs = do
  pkh <- pubKeyHash <$> ownPubKey

  cs <- case mcs of
    Nothing ->
      fmap Currency.currencySymbol $
        mapError (pack . show @Currency.CurrencyError) $
          Currency.mintContract pkh [(uniswapTokenName, 1)]
    Just jcs -> return jcs
  let c = mkCoin cs uniswapTokenName
      us = uniswap cs -- NFT token for a given uniswap instance
      inst = uniswapInstance us
      tx = mustPayToTheScript (Factory []) $ unitValue c
  ledgerTx <- submitTxConstraints inst tx
  void $ awaitTxConfirmed $ txId ledgerTx
  logInfo @String $ printf "started Uniswap %s at address %s" (show us) (show $ uniswapAddress us)
  return us

-- | Creates a liquidity pool for a pair of coins. The creator provides liquidity for both coins and gets liquidity tokens in return.
create :: Uniswap -> CreateParams -> Contract w s Text ()
create us CreateParams {..} = do
  when (unCoin coinA == unCoin coinB) $ throwError "coins must be different"
  when (amountA <= 0 || amountB <= 0) $ throwError "amounts must be positive"
  (oref, o, lps) <- findUniswapFactory us
  let liquidity = calculateInitialLiquidity amountA amountB
      lp = liquidityPool (coinA, coinB) fee
  let usInst = uniswapInstance us
      usScript = uniswapScript us
      usDat1 = Factory $ lp : lps
      usDat2 = Pool lp liquidity
      psC = poolStateCoin us
      lC = mkCoin (liquidityCurrency us) $ lpTicker lp
      usVal = unitValue $ usCoin us
      lpVal = valueOf coinA amountA <> valueOf coinB amountB <> unitValue psC

      lookups =
        Constraints.typedValidatorLookups usInst
          <> Constraints.otherScript usScript
          <> Constraints.mintingPolicy (liquidityPolicy us)
          <> Constraints.unspentOutputs (Map.singleton oref o)

      tx =
        Constraints.mustPayToTheScript usDat1 usVal
          <> Constraints.mustPayToTheScript usDat2 lpVal
          <> Constraints.mustMintValue (unitValue psC <> valueOf lC liquidity)
          <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Create lp)

  void $ submitTxConstraintsWith lookups tx
  logInfo $ "created liquidity pool: " ++ show lp

-- | Closes a liquidity pool by burning all remaining liquidity tokens in exchange for all liquidity remaining in the pool.
close :: Uniswap -> CloseParams -> Contract w s Text ()
close us CloseParams {..} = do
  ((oref1, o1, lps), (oref2, o2, lp, liquidity)) <- findUniswapFactoryAndPool us coinA coinB fee
  pkh <- pubKeyHash <$> ownPubKey
  let usInst = uniswapInstance us
      usScript = uniswapScript us
      usDat = Factory $ filter (/= lp) lps
      usC = usCoin us
      psC = poolStateCoin us
      lC = mkCoin (liquidityCurrency us) $ lpTicker lp
      usVal = unitValue usC
      psVal = unitValue psC
      lVal = valueOf lC liquidity
      redeemer = Redeemer $ PlutusTx.toBuiltinData Close

      lookups =
        Constraints.typedValidatorLookups usInst
          <> Constraints.otherScript usScript
          <> Constraints.mintingPolicy (liquidityPolicy us)
          <> Constraints.ownPubKeyHash pkh
          <> Constraints.unspentOutputs (Map.singleton oref1 o1 <> Map.singleton oref2 o2)

      tx =
        Constraints.mustPayToTheScript usDat usVal
          <> Constraints.mustMintValue (negate $ psVal <> lVal)
          <> Constraints.mustSpendScriptOutput oref1 redeemer
          <> Constraints.mustSpendScriptOutput oref2 redeemer
          <> Constraints.mustIncludeDatum (Datum $ PlutusTx.toBuiltinData $ Pool lp liquidity)

  void $ submitTxConstraintsWith lookups tx
  logInfo $ "closed liquidity pool: " ++ show lp

-- | Removes some liquidity from a liquidity pool in exchange for liquidity tokens.
remove :: Uniswap -> RemoveParams -> Contract w s Text ()
remove us RemoveParams {..} = do
  (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us coinA coinB fee
  pkh <- pubKeyHash <$> ownPubKey
  when (diff < 1 || diff >= liquidity) $ throwError "removed liquidity must be positive and less than total liquidity"
  let usInst = uniswapInstance us
      usScript = uniswapScript us
      dat = Pool lp $ liquidity - diff
      psC = poolStateCoin us
      lC = mkCoin (liquidityCurrency us) $ lpTicker lp
      psVal = unitValue psC
      lVal = valueOf lC diff
      inVal = view ciTxOutValue o
      inA = amountOf inVal coinA
      inB = amountOf inVal coinB
      (outA, outB) = calculateRemoval inA inB liquidity diff
      val = psVal <> valueOf coinA outA <> valueOf coinB outB
      redeemer = Redeemer $ PlutusTx.toBuiltinData Remove

      lookups =
        Constraints.typedValidatorLookups usInst
          <> Constraints.otherScript usScript
          <> Constraints.mintingPolicy (liquidityPolicy us)
          <> Constraints.unspentOutputs (Map.singleton oref o)
          <> Constraints.ownPubKeyHash pkh

      tx =
        Constraints.mustPayToTheScript dat val
          <> Constraints.mustMintValue (negate lVal)
          <> Constraints.mustSpendScriptOutput oref redeemer

  void $ submitTxConstraintsWith lookups tx
  logInfo $ "removed liquidity from pool: " ++ show lp

-- | Adds some liquidity to an existing liquidity pool in exchange for newly minted liquidity tokens.
add :: Uniswap -> AddParams -> Contract w s Text ()
add us AddParams {..} = do
  pkh <- pubKeyHash <$> ownPubKey
  (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us coinA coinB fee
  logInfo @String $ printf "old liquidity = %d" $ unAmount liquidity
  when (amountA < 0 || amountB < 0) $ throwError "amounts must not be negative"
  let outVal = view ciTxOutValue o
      oldA = amountOf outVal coinA
      oldB = amountOf outVal coinB
      newA = oldA + amountA
      newB = oldB + amountB
      delL = calculateAdditionalLiquidity oldA oldB liquidity amountA amountB
      inVal = valueOf coinA amountA <> valueOf coinB amountB
  when (delL <= 0) $ throwError "insufficient liquidity"
  logInfo @String $ printf "oldA = %d, oldB = %d, newA = %d, newB = %d, delL = %d" oldA oldB newA newB delL

  let usInst = uniswapInstance us
      usScript = uniswapScript us
      dat = Pool lp $ liquidity + delL
      psC = poolStateCoin us
      lC = mkCoin (liquidityCurrency us) $ lpTicker lp
      psVal = unitValue psC
      lVal = valueOf lC delL
      val = psVal <> valueOf coinA newA <> valueOf coinB newB
      redeemer = Redeemer $ PlutusTx.toBuiltinData Add

      lookups =
        Constraints.typedValidatorLookups usInst
          <> Constraints.otherScript usScript
          <> Constraints.mintingPolicy (liquidityPolicy us)
          <> Constraints.ownPubKeyHash pkh
          <> Constraints.unspentOutputs (Map.singleton oref o)

      tx =
        Constraints.mustPayToTheScript dat val
          <> Constraints.mustMintValue lVal
          <> Constraints.mustSpendScriptOutput oref redeemer

  logInfo @String $ printf "val = %s, inVal = %s" (show val) (show inVal)
  logInfo $ show lookups
  logInfo $ show tx

  void $ submitTxConstraintsWith lookups tx
  logInfo $ "added liquidity to pool: " ++ show lp

swapPreview :: Uniswap -> SwapPreviewParams -> Contract w s Text SwapPreviewResultData
swapPreview us SwapPreviewParams {..} = do
  (_, (_, o, _, _)) <- findUniswapFactoryAndPool us coinA coinB fee
  let outVal = view ciTxOutValue o
  let oldA = amountOf outVal coinA
      oldB = amountOf outVal coinB
  let outB = Amount $ findSwap oldA oldB amount fee
  return $ SwapPreviewResultData coinA amount coinB outB fee

-- | Uses a liquidity pool two swap one sort of coins in the pool against the other.
swap :: Uniswap -> SwapParams -> Contract w s Text ()
swap us SwapParams {..} = do
  unless (amount > 0) $ throwError "amount must be positive"
  (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us coinA coinB fee
  let outVal = view ciTxOutValue o
  let oldA = amountOf outVal coinA
      oldB = amountOf outVal coinB
  (newA, newB, outB) <- do
    let outB = Amount $ findSwap oldA oldB amount fee
    when (outB == 0) $ throwError "no payout"
    return (oldA + amount, oldB - outB, outB)

  pkh <- pubKeyHash <$> ownPubKey

  when ((100 * unAmount outB `div` unAmount result) < (100 - slippage)) $
    throwError "outB amount doesn't meet the slippage criteria"
  logInfo @String $ printf "oldA = %d, oldB = %d, old product = %d, newA = %d, newB = %d, new product = %d" oldA oldB (unAmount oldA * unAmount oldB) newA newB (unAmount newA * unAmount newB)

  let inst = uniswapInstance us
      val = valueOf coinA newA <> valueOf coinB newB <> unitValue (poolStateCoin us)

      lookups =
        Constraints.typedValidatorLookups inst
          <> Constraints.otherScript (Scripts.validatorScript inst)
          <> Constraints.unspentOutputs (Map.singleton oref o)
          <> Constraints.ownPubKeyHash pkh

      tx =
        Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Swap)
          <> Constraints.mustPayToTheScript (Pool lp liquidity) val

  void $ submitTxConstraintsWith lookups tx
  logInfo $ "swapped with: " ++ show lp

indirectSwapPreview :: Uniswap -> ISwapPreviewParams -> Contract w s Text ISwapPreviewResultData
indirectSwapPreview us ISwapPreviewParams {..} = do
  abstractPools <- uniquePools us
  (_, outB) <- case findBestSwap abstractPools (unCoin coinA, unCoin coinB) (unAmount amount) of
    Nothing -> throwError "No path found"
    Just p -> return p
  return $ ISwapPreviewResultData coinA amount coinB (Amount outB)

uniquePoolsWithCoins :: Uniswap -> Contract w s Text [LiquidityPoolWithCoins]
uniquePoolsWithCoins us = do
  (_, _, allPools) <- findUniswapFactory us
  relevantPools <- mapM (\lp -> findUniswapPool us lp >>= \x -> pure (x, lp)) allPools
  let uniquePools' =
        map (\((a, b, f), (amountA, amountB)) -> LiquidityPoolWithCoins (Coin a) (Coin b) f (Amount amountA) (Amount amountB)) $
          distinctPools $
            map
              ( \((_, pO, _), LiquidityPool {..}) ->
                  let pOutVal = view ciTxOutValue pO
                      amountA = unAmount $ amountOf pOutVal lpCoinA
                      amountB = unAmount $ amountOf pOutVal lpCoinB
                   in ((unCoin lpCoinA, unCoin lpCoinB, lpFee), (amountA, amountB))
              )
              relevantPools
  return uniquePools'
  where
    distinctPools :: [((AssetClass, AssetClass, Fee), (Integer, Integer))] -> [((AssetClass, AssetClass, Fee), (Integer, Integer))]
    distinctPools = nubBy eqPool
      where
        eqPool ((a1, a2, f1), _) ((b1, b2, f2), _) = f1 == f2 && ((a1, a2) == (b1, b2) || (a1, a2) == (b2, b1))

uniquePools :: Uniswap -> Contract w s Text (Map.Map (AssetClass, AssetClass, Fee) (Integer, Integer))
uniquePools us = do
  (_, _, allPools) <- findUniswapFactory us
  relevantPools <- mapM (\lp -> findUniswapPool us lp >>= \x -> pure (x, lp)) allPools
  let uniquePools' =
        Map.fromList $
          distinctPools $
            map
              ( \((_, pO, _), LiquidityPool {..}) ->
                  let pOutVal = view ciTxOutValue pO
                      amountA = unAmount $ amountOf pOutVal lpCoinA
                      amountB = unAmount $ amountOf pOutVal lpCoinB
                   in ((unCoin lpCoinA, unCoin lpCoinB, lpFee), (amountA, amountB))
              )
              relevantPools
  return uniquePools'
  where
    distinctPools :: [((AssetClass, AssetClass, Fee), (Integer, Integer))] -> [((AssetClass, AssetClass, Fee), (Integer, Integer))]
    distinctPools = nubBy eqPool
      where
        eqPool ((a1, a2, f1), _) ((b1, b2, f2), _) = f1 == f2 && ((a1, a2) == (b1, b2) || (a1, a2) == (b2, b1))

-- indirectSwap2 :: forall w s. Uniswap -> IndirectSwapParams -> Contract w s Text ()
-- indirectSwap2 us IndirectSwapParams {..} =
--   runM $
--     translate
--       ( \case
--           IndirectSwapFail message -> throwError message
--           IndirectSwapPools -> uniquePoolsWithCoins us
--           IndirectSwapStep (unspentOutputs, constraints) (LiquidityPoolWithCoins {..}, (inAmount, outAmount)) -> do
--             (oref, o, liquidity) <- findUniswapPool us $ liquidityPool (coinA, coinB) fee
--             let (newA, newB) = (amountA + inAmount, amountB - outAmount)
--             let val = valueOf coinA newA <> valueOf coinB newB <> unitValue (poolStateCoin us)
--             return
--               ( Map.insert oref o unspentOutputs,
--                 constraints
--                   <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Swap)
--                   <> Constraints.mustPayToTheScript (Pool (liquidityPool (coinA, coinB) fee) liquidity) val
--               )
--           IndirectSwapInitStepState -> return (Map.empty, mempty)
--           IndirectSwapCommit (unspentOutputs, constraints) ->
--             do
--               pkh <- pubKeyHash <$> ownPubKey
--               let inst = uniswapInstance us

--                   lookups =
--                     Constraints.typedValidatorLookups inst
--                       <> Constraints.otherScript (Scripts.validatorScript inst)
--                       <> Constraints.unspentOutputs unspentOutputs
--                       <> Constraints.ownPubKeyHash pkh

--               void $ submitTxConstraintsWith lookups constraints
--           IndirectSwapLog message -> logInfo @Text message
--       )
--       (iswap @(Map.Map TxOutRef TxOutTx, TxConstraints UniswapAction UniswapDatum) IndirectSwapParams {..})

indirectSwap :: Uniswap -> IndirectSwapParams -> Contract w s Text ()
indirectSwap us IndirectSwapParams {..} = do
  unless (amount > 0) $ throwError "amount must be positive"
  (_, _, allPools) <- findUniswapFactory us
  relevantPools <- mapM (\lp -> findUniswapPool us lp >>= \x -> pure (x, lp)) allPools
  abstractPools <- uniquePools us
  let orefs = Map.fromList $ map (\((pRef, pO, _), LiquidityPool {..}) -> ((unCoin lpCoinA, unCoin lpCoinB, lpFee), (pRef, pO))) relevantPools
  let myPools = Map.fromList $ map (\((_, _, liquidity), pool@LiquidityPool {..}) -> ((unCoin lpCoinA, unCoin lpCoinB, lpFee), (pool, liquidity))) relevantPools

  (path, outB) <- case findBestSwap abstractPools (unCoin coinA, unCoin coinB) (unAmount amount) of
    Nothing -> throwError "No path found"
    Just p -> return p

  when ((100 * outB `div` unAmount result) < (100 - slippage)) $
    throwError "outB amount doesn't meet the slippage criteria"

  logInfo @String (show path)
  let moneyToPay = zip path $ scanl (\amount (a, b, fee) -> let Just (a', b') = Map.lookup (a, b, fee) abstractPools <|> ((\(x, y) -> (y, x)) <$> Map.lookup (b, a, fee) abstractPools) in (Amount $ findSwap (Amount a') (Amount b') amount fee)) amount path
  pkh <- pubKeyHash <$> ownPubKey

  let inst = uniswapInstance us

      lookups =
        Constraints.typedValidatorLookups inst
          <> Constraints.otherScript (Scripts.validatorScript inst)
          <> Constraints.unspentOutputs (Map.fromList $ map (\((oref, o, _), _) -> (oref, o)) relevantPools)
          <> Constraints.ownPubKeyHash pkh

      tx =
        mconcat $
          map
            ( \((a, b, fee), amount) ->
                let Just (oref, _) = Map.lookup (a, b, fee) orefs <|> Map.lookup (b, a, fee) orefs
                    Just (pool, liquidity) = Map.lookup (a, b, fee) myPools <|> Map.lookup (b, a, fee) myPools
                    Just (oldA, oldB) = Map.lookup (a, b, fee) abstractPools <|> (let Just (b', a') = Map.lookup (b, a, fee) abstractPools in Just (a', b'))
                    (newA, newB) = (oldA + unAmount amount, oldB - findSwap (Amount oldA) (Amount oldB) amount fee)

                    val = valueOf (Coin a) (Amount newA) <> valueOf (Coin b) (Amount newB) <> unitValue (poolStateCoin us)
                 in Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Swap)
                      <> Constraints.mustPayToTheScript (Pool pool liquidity) val
            )
            moneyToPay

  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx

-- | Finds all liquidity pools and their liquidity belonging to the Uniswap instance.
-- This merely inspects the blockchain and does not issue any transactions.
pools :: forall w s. Uniswap -> Contract w s Text [LiquidityPoolView]
pools us = do
  utxos <- utxosAt (uniswapAddress us)
  go $ snd <$> Map.toList utxos
  where
    go :: [ChainIndexTxOut] -> Contract w s Text [LiquidityPoolView]
    go [] = return []
    go (o : os) = do
        let v = view ciTxOutValue o
        if isUnity v c
        then do
          d <- getUniswapDatum o
          case d of
            Factory _ -> go os
            Pool lp _ -> do
              let coinA = lpCoinA lp
                  coinB = lpCoinB lp
                  amtA = amountOf v coinA
                  amtB = amountOf v coinB
                  fee = lpFee lp
                  lC = mkCoin (liquidityCurrency us) $ lpTicker lp
                  s = LiquidityPoolView coinA coinB fee amtA amtB lC
              logInfo $ "found pool: " ++ show s
              ss <- go os
              return $ s : ss
        else go os
      where
        c :: Coin PoolState
        c = poolStateCoin us

-- | Gets the caller's funds.
funds :: Contract w s Text [AmountOfCoin A]
funds = do
  pkh <- pubKeyHash <$> ownPubKey
  os <- map snd . Map.toList <$> utxosAt (pubKeyHashAddress pkh)
  let v = getValue $ mconcat [view ciTxOutValue o | o <- os]
  return [AmountOfCoin (mkCoin cs tn) (Amount a) | (cs, tns) <- AssocMap.toList v, (tn, a) <- AssocMap.toList tns]

clearState :: ClearStateParams -> Contract (History (Either Text UserContractState)) s Text ()
clearState ClearStateParams {..} = do
  tell $ WH.remove removeId


getUniswapDatum :: ChainIndexTxOut -> Contract w s Text UniswapDatum
getUniswapDatum o =
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


findUniswapInstance ::
    forall a b w s.
    Uniswap
    -> Coin b
    -> (UniswapDatum -> Maybe a)
    -> Contract w s Text (TxOutRef, ChainIndexTxOut, a)
findUniswapInstance us c f = do
    let addr = uniswapAddress us
    logInfo @String $ printf "looking for Uniswap instance at address %s containing coin %s " (show addr) (show c)
    utxos <- utxosAt addr
    go  [x | x@(_, o) <- Map.toList utxos, isUnity (view ciTxOutValue o) c]
  where
    go [] = throwError "Uniswap instance not found"
    go ((oref, o) : xs) = do
        d <- getUniswapDatum o
        case f d of
            Nothing -> go xs
            Just a  -> do
                logInfo @String $ printf "found Uniswap instance with datum: %s" (show d)
                return (oref, o, a)


findUniswapFactory :: forall w s. Uniswap -> Contract w s Text (TxOutRef, ChainIndexTxOut, [LiquidityPool])
findUniswapFactory us@Uniswap{..} = findUniswapInstance us usCoin $ \case
    Factory lps -> Just lps
    Pool _ _    -> Nothing

findUniswapPool :: Uniswap -> LiquidityPool -> Contract w s Text (TxOutRef, ChainIndexTxOut, Amount Liquidity)
findUniswapPool us lp = findUniswapInstance us (poolStateCoin us) $ \case
  Pool lp' l
    | lp == lp' -> Just l
  _ -> Nothing

findUniswapFactoryAndPool ::
  Uniswap ->
  Coin A ->
  Coin B ->
  (Integer, Integer) ->
  Contract
    w
    s
    Text
    ( (TxOutRef, ChainIndexTxOut, [LiquidityPool]),
      (TxOutRef, ChainIndexTxOut, LiquidityPool, Amount Liquidity)
    )
findUniswapFactoryAndPool us coinA coinB fee = do
  (oref1, o1, lps) <- findUniswapFactory us
  case [ lp'
         | lp' <- lps,
           lp' == liquidityPool (coinA, coinB) fee
       ] of
    [lp] -> do
      (oref2, o2, a) <- findUniswapPool us lp
      return
        ( (oref1, o1, lps),
          (oref2, o2, lp, a)
        )
    _ -> throwError "liquidity pool not found"

ownerEndpoint :: Promise (History (Either Text Uniswap)) UniswapOwnerSchema Void ()
ownerEndpoint = startHandler <> ownerEndpoint
  where
    startHandler :: Promise (History (Either Text Uniswap)) UniswapOwnerSchema Void ()
    startHandler =
      handleEndpoint @"start" @_ @_ @_ @Text
        ( \case
            (Right (WithHistoryId guid _)) -> do
              e <- runError (start Nothing)
              case e of
                Left err -> tell $ WH.append guid $ Left err
                Right uniswap' -> tell $ WH.append guid $ Right uniswap'
        )

ownerEndpoint' :: Promise (History (Either Text Uniswap)) UniswapOwnerSchema' Void ()
ownerEndpoint' = startHandler <> ownerEndpoint'
  where
    startHandler :: Promise (History (Either Text Uniswap)) UniswapOwnerSchema' Void ()
    startHandler =
      handleEndpoint @"start" @_ @_ @_ @Text
        ( \case
            (Right (WithHistoryId guid currency)) -> do
              e <- runError (start (Just currency))
              case e of
                Left err -> tell $ WH.append guid $ Left err
                Right uniswap' -> tell $ WH.append guid $ Right uniswap'
        )

-- | Provides the following endpoints for users of a Uniswap instance:
--
--      [@create@]: Creates a liquidity pool for a pair of coins. The creator provides liquidity for both coins and gets liquidity tokens in return.
--      [@swap@]: Uses a liquidity pool two swap one sort of coins in the pool against the other.
--      [@close@]: Closes a liquidity pool by burning all remaining liquidity tokens in exchange for all liquidity remaining in the pool.
--      [@remove@]: Removes some liquidity from a liquidity pool in exchange for liquidity tokens.
--      [@add@]: Adds some liquidity to an existing liquidity pool in exchange for newly minted liquidity tokens.
--      [@pools@]: Finds all liquidity pools and their liquidity belonging to the Uniswap instance. This merely inspects the blockchain and does not issue any transactions.
--      [@funds@]: Gets the caller's funds. This merely inspects the blockchain and does not issue any transactions.
--      [@stop@]: Stops the contract.
userEndpoints :: Uniswap -> Promise (History (Either Text UserContractState)) UniswapUserSchema Void ()
userEndpoints us =
  stop
    `select` ( ( f (Proxy @"create") historyId (const Created) (\us WithHistoryId {..} -> create us content)
                   `select` f (Proxy @"swap") historyId (const Swapped) (\us WithHistoryId {..} -> swap us content)
                   `select` f (Proxy @"swapPreview") historyId SwapPreviewResult (\us WithHistoryId {..} -> swapPreview us content)
                   `select` f (Proxy @"iSwap") historyId (const ISwapped) (\us WithHistoryId {..} -> indirectSwap us content)
                   `select` f (Proxy @"iSwapPreview") historyId ISwapPreviewResult (\us WithHistoryId {..} -> indirectSwapPreview us content)
                   `select` f (Proxy @"close") historyId (const Closed) (\us WithHistoryId {..} -> close us content)
                   `select` f (Proxy @"remove") historyId (const Removed) (\us WithHistoryId {..} -> remove us content)
                   `select` f (Proxy @"add") historyId (const Added) (\us WithHistoryId {..} -> add us content)
                   `select` f (Proxy @"pools") historyId AvailablePools (\us _ -> pools us)
                   `select` f (Proxy @"funds") historyId AvailableFunds (\_us _ -> funds)
                   `select` f (Proxy @"clearState") historyId (const Cleared) (\us WithHistoryId {..} -> clearState content)
               )
                 <> userEndpoints us
             )
  where
    f ::
      forall l a p.
      (HasEndpoint l p UniswapUserSchema, FromJSON p) =>
      Proxy l ->
      (p -> Text) ->
      (a -> UserContractState) ->
      (Uniswap -> p -> Contract (History (Either Text UserContractState)) UniswapUserSchema Text a) ->
      Promise (History (Either Text UserContractState)) UniswapUserSchema Void ()
    f _ getGuid g c = handleEndpoint @l $ \p -> do
      let guid = either (const "ERROR") getGuid p
      e <- either (pure . Left) (runError @_ @_ @Text . c us) p

      case e of
        Left err -> do
          logInfo @Text ("Error during calling endpoint: " <> err)
          tell $ WH.append guid . Left $ err
        Right a
          | symbolVal (Proxy @l) GHC.Classes./= "clearState" ->
            tell $ WH.append guid . Right . g $ a
        _ -> return ()

    stop :: Promise (History (Either Text UserContractState)) UniswapUserSchema Void ()
    stop = handleEndpoint @"stop" $ \e -> do
      tell $ case e of
        Left err -> WH.append "ERROR" $ Left err
        Right (WithHistoryId hId _) -> WH.append hId $ Right Stopped
