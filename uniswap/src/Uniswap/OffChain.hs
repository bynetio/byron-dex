{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
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

module Uniswap.OffChain
  ( poolStateCoinFromUniswapCurrency,
    liquidityCoin,
    CreateParams (..),
    SwapParams (..),
    SwapPreviewParams (..),
    IndirectSwapParams (..),
    ISwapPreviewParams (..),
    CloseParams (..),
    RemoveParams (..),
    AddParams (..),
    FundsParams (..),
    PoolsParams (..),
    StopParams (..),
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
  )
where

import           Control.Applicative          ((<|>))
import           Control.Monad                hiding (fmap, mapM, mapM_)
import           Data.List                    (scanl)
import qualified Data.Map                     as Map
import           Data.Proxy                   (Proxy (..))
import           Data.Text                    (Text, pack)
import           Data.Void                    (Void)
import           Ledger                       hiding (fee, singleton)
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Playground.Contract
import           Plutus.Contract
import qualified Plutus.Contracts.Currency    as Currency
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup (..), unless)
import           Prelude                      (Semigroup (..), String, div,
                                               show)
import           Text.Printf                  (printf)
import           Uniswap.Common.WalletHistory (History, HistoryId)
import qualified Uniswap.Common.WalletHistory as WH
import           Uniswap.IndirectSwaps
import           Uniswap.OnChain              (mkUniswapValidator,
                                               validateLiquidityForging)
import           Uniswap.Pool
import           Uniswap.Types
data Uniswapping

instance Scripts.ValidatorTypes Uniswapping where
  type RedeemerType Uniswapping = UniswapAction
  type DatumType Uniswapping = UniswapDatum

type UniswapOwnerSchema =
    Endpoint "start" HistoryId

type UniswapOwnerSchema' =
    Endpoint "start" (HistoryId,CurrencySymbol)



-- | Schema for the endpoints for users of Uniswap.
type UniswapUserSchema =
    Endpoint "create" CreateParams
    .\/ Endpoint "swap" SwapParams
    .\/ Endpoint "swapPreview" SwapPreviewParams
    .\/ Endpoint "iSwapPreview" ISwapPreviewParams
    .\/ Endpoint "iSwap" IndirectSwapParams
    .\/ Endpoint "close" CloseParams
    .\/ Endpoint "remove" RemoveParams
    .\/ Endpoint "add" AddParams
    .\/ Endpoint "pools" PoolsParams
    .\/ Endpoint "funds" FundsParams
    .\/ Endpoint "stop" StopParams
    .\/ Endpoint "clearState" ClearStateParams

-- | Type of the Uniswap user contract state.
data UserContractState
  = Pools [((Coin A, Amount A), (Coin B, Amount B), Fee)]
  | Funds Value
  | Created
  | Swapped
  | SwapPreview ((Coin A, Amount A), (Coin B, Amount B), Fee)
  | ISwapped
  | ISwapPreview ((Coin A, Amount A), (Coin B, Amount B))
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
  Scripts.mkTypedValidator  @Uniswapping
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

liquidityPolicy :: Uniswap -> MonetaryPolicy
liquidityPolicy us =
  mkMonetaryPolicyScript $
    $$(PlutusTx.compile [||\u t -> Scripts.wrapMonetaryPolicy (validateLiquidityForging u t)||])
      `PlutusTx.applyCode` PlutusTx.liftCode us
      `PlutusTx.applyCode` PlutusTx.liftCode poolStateTokenName

liquidityCurrency :: Uniswap -> CurrencySymbol
liquidityCurrency = scriptCurrencySymbol . liquidityPolicy

poolStateCoin :: Uniswap -> Coin PoolState
poolStateCoin = flip mkCoin poolStateTokenName . liquidityCurrency

-- | Gets the 'Coin' used to identity liquidity pools.
poolStateCoinFromUniswapCurrency ::
  -- | The currency identifying the Uniswap instance.
  CurrencySymbol ->
  Coin PoolState
poolStateCoinFromUniswapCurrency = poolStateCoin . uniswap

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

-- | Parameters for the @create@-endpoint, which creates a new liquidity pool.
data CreateParams = CreateParams
  { -- | Unique Identifier of Operation
    cpOpId    :: HistoryId,
    -- | One 'Coin' of the liquidity pair.
    cpCoinA   :: Coin A,
    -- | The other 'Coin'.
    cpCoinB   :: Coin B,
    -- | Numerator and denominator of the swap fee
    cpFee     :: Fee,
    -- | Amount of liquidity for the first 'Coin'.
    cpAmountA :: Amount A,
    -- | Amount of liquidity for the second 'Coin'.
    cpAmountB :: Amount B
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @swap@-endpoint, which allows swaps between the two different coins in a liquidity pool.
-- One of the provided amounts must be positive, the other must be zero.
data SwapParams = SwapParams
  { -- | Unique Identifier of Operation
    spOpId     :: HistoryId,
    -- | One 'Coin' of the liquidity pair.
    spCoinA    :: Coin A,
    -- | The other 'Coin'.
    spCoinB    :: Coin B,
    -- | Numerator and denominator of the swap fee
    spFee      :: Fee,
    -- | The amount the first 'Coin' that should be swapped.
    spAmount   :: Amount A,
    -- | The expected amount of swaped 'Coin B' (quoted amount)
    spResult   :: Amount B,
    -- | The expected % difference between quoted and executed prices.
    spSlippage :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data SwapPreviewParams = SwapPreviewParams
  { -- | Unique Identifier of Operation
    sppOpId   :: HistoryId,
    sppCoinA  :: Coin A,
    sppCoinB  :: Coin B,
    -- | Numerator and denominator of the swap fee
    sppFee    :: Fee,
    sppAmount :: Amount A
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data IndirectSwapParams = IndirectSwapParams
  { -- | Unique Identifier of Operation
    ispOpId     :: HistoryId,
    -- | One 'Coin' of the liquidity pair.
    ispCoinA    :: Coin A,
    -- | The other 'Coin'.
    ispCoinB    :: Coin B,
    -- | The amount of the first 'Coin' that should be swapped.
    ispAmount   :: Amount A,
    ispResult   :: Amount B,
    ispSlippage :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data ISwapPreviewParams = ISwapPreviewParams
  { -- | Unique Identifier of Operation
    isppOpId   :: HistoryId,
    isppCoinA  :: Coin A,
    isppCoinB  :: Coin B,
    isppAmount :: Amount A
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @close@-endpoint, which closes a liquidity pool.
data CloseParams = CloseParams
  { -- | Unique Identifier of Operation
    clpOpId  :: HistoryId,
    -- | One 'Coin' of the liquidity pair.
    clpCoinA :: Coin A,
    -- | The other 'Coin' of the liquidity pair.
    clpCoinB :: Coin B,
    -- | Numerator and denominator of the swap fee
    clpFee   :: Fee
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @remove@-endpoint, which removes some liquidity from a liquidity pool.
data RemoveParams = RemoveParams
  { -- | Unique Identifier of Operation
    rpOpId  :: HistoryId,
     -- | One 'Coin' of the liquidity pair.
    rpCoinA :: Coin A,
    -- | The other 'Coin' of the liquidity pair.
    rpCoinB :: Coin B,
    -- | Numerator and denominator of the swap fee
    rpFee   :: Fee,
    -- | The amount of liquidity tokens to burn in exchange for liquidity from the pool.
    rpDiff  :: Amount Liquidity
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @add@-endpoint, which adds liquidity to a liquidity pool in exchange for liquidity tokens.
data AddParams = AddParams
  { -- | Unique Identifier of Operation
    apOpId    :: HistoryId,
     -- | One 'Coin' of the liquidity pair.
    apCoinA   :: Coin A,
    -- | The other 'Coin' of the liquidity pair.
    apCoinB   :: Coin B,
    -- | Numerator and denominator of the swap fee
    apFee     :: Fee,
    -- | The amount of coins of the first kind to add to the pool.
    apAmountA :: Amount A,
    -- | The amount of coins of the second kind to add to the pool.
    apAmountB :: Amount B
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @pools-@endpoint, which lookups available pools
data PoolsParams = PoolsParams
  { -- | Unique Identifier of Operation
    plOpId :: HistoryId
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @funds-@endpoint, which lookups funds of the current wallet
data FundsParams = FundsParams
  { -- | Unique Identifier of Operation
    fsOpId :: HistoryId
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @stop-@endpoint, which stops the uniswap instance
data StopParams = StopParams
  { -- | Unique Identifier of Operation
    stOpId :: HistoryId
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @clearState-@endpoint, which removes entry from the state corresponding to given HistoryId
data ClearStateParams = ClearStateParams
  { -- | Unique Identifier of Operation
    clsOpId     :: HistoryId,
    -- | Identifier of Operation that should be removed from state
    clsRemoveId :: HistoryId
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)


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
          Currency.forgeContract pkh [(uniswapTokenName, 1)]
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
  when (unCoin cpCoinA == unCoin cpCoinB) $ throwError "coins must be different"
  when (cpAmountA <= 0 || cpAmountB <= 0) $ throwError "amounts must be positive"
  (oref, o, lps) <- findUniswapFactory us
  let liquidity = calculateInitialLiquidity cpAmountA cpAmountB
      lp = liquidityPool (cpCoinA, cpCoinB) cpFee
  let usInst = uniswapInstance us
      usScript = uniswapScript us
      usDat1 = Factory $ lp : lps
      usDat2 = Pool lp liquidity
      psC = poolStateCoin us
      lC = mkCoin (liquidityCurrency us) $ lpTicker lp
      usVal = unitValue $ usCoin us
      lpVal = valueOf cpCoinA cpAmountA <> valueOf cpCoinB cpAmountB <> unitValue psC

      lookups =
        Constraints.typedValidatorLookups usInst
          <> Constraints.otherScript usScript
          <> Constraints.monetaryPolicy (liquidityPolicy us)
          <> Constraints.unspentOutputs (Map.singleton oref o)

      tx =
        Constraints.mustPayToTheScript usDat1 usVal
          <> Constraints.mustPayToTheScript usDat2 lpVal
          <> Constraints.mustForgeValue (unitValue psC <> valueOf lC liquidity)
          <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData $ Create lp)

  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx

  logInfo $ "created liquidity pool: " ++ show lp

-- | Closes a liquidity pool by burning all remaining liquidity tokens in exchange for all liquidity remaining in the pool.
close :: Uniswap -> CloseParams -> Contract w s Text ()
close us CloseParams {..} = do
  ((oref1, o1, lps), (oref2, o2, lp, liquidity)) <- findUniswapFactoryAndPool us clpCoinA clpCoinB clpFee
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
      redeemer = Redeemer $ PlutusTx.toData Close

      lookups =
        Constraints.typedValidatorLookups usInst
          <> Constraints.otherScript usScript
          <> Constraints.monetaryPolicy (liquidityPolicy us)
          <> Constraints.ownPubKeyHash pkh
          <> Constraints.unspentOutputs (Map.singleton oref1 o1 <> Map.singleton oref2 o2)

      tx =
        Constraints.mustPayToTheScript usDat usVal
          <> Constraints.mustForgeValue (negate $ psVal <> lVal)
          <> Constraints.mustSpendScriptOutput oref1 redeemer
          <> Constraints.mustSpendScriptOutput oref2 redeemer
          <> Constraints.mustIncludeDatum (Datum $ PlutusTx.toData $ Pool lp liquidity)

  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx

  logInfo $ "closed liquidity pool: " ++ show lp

-- | Removes some liquidity from a liquidity pool in exchange for liquidity tokens.
remove :: Uniswap -> RemoveParams -> Contract w s Text ()
remove us RemoveParams {..} = do
  (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us rpCoinA rpCoinB rpFee
  pkh <- pubKeyHash <$> ownPubKey
  when (rpDiff < 1 || rpDiff >= liquidity) $ throwError "removed liquidity must be positive and less than total liquidity"
  let usInst = uniswapInstance us
      usScript = uniswapScript us
      dat = Pool lp $ liquidity - rpDiff
      psC = poolStateCoin us
      lC = mkCoin (liquidityCurrency us) $ lpTicker lp
      psVal = unitValue psC
      lVal = valueOf lC rpDiff
      inVal = txOutValue $ txOutTxOut o
      inA = amountOf inVal rpCoinA
      inB = amountOf inVal rpCoinB
      (outA, outB) = calculateRemoval inA inB liquidity rpDiff
      val = psVal <> valueOf rpCoinA outA <> valueOf rpCoinB outB
      redeemer = Redeemer $ PlutusTx.toData Remove

      lookups =
        Constraints.typedValidatorLookups usInst
          <> Constraints.otherScript usScript
          <> Constraints.monetaryPolicy (liquidityPolicy us)
          <> Constraints.unspentOutputs (Map.singleton oref o)
          <> Constraints.ownPubKeyHash pkh

      tx =
        Constraints.mustPayToTheScript dat val
          <> Constraints.mustForgeValue (negate lVal)
          <> Constraints.mustSpendScriptOutput oref redeemer

  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx

  logInfo $ "removed liquidity from pool: " ++ show lp

-- | Adds some liquidity to an existing liquidity pool in exchange for newly minted liquidity tokens.
add :: Uniswap -> AddParams -> Contract w s Text ()
add us AddParams {..} = do
  pkh <- pubKeyHash <$> ownPubKey
  (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us apCoinA apCoinB apFee
  logInfo @String $ printf "old liquidity = %d" $ unAmount liquidity
  when (apAmountA < 0 || apAmountB < 0) $ throwError "amounts must not be negative"
  let outVal = txOutValue $ txOutTxOut o
      oldA = amountOf outVal apCoinA
      oldB = amountOf outVal apCoinB
      newA = oldA + apAmountA
      newB = oldB + apAmountB
      delL = calculateAdditionalLiquidity oldA oldB liquidity apAmountA apAmountB
      inVal = valueOf apCoinA apAmountA <> valueOf apCoinB apAmountB
  when (delL <= 0) $ throwError "insufficient liquidity"
  logInfo @String $ printf "oldA = %d, oldB = %d, newA = %d, newB = %d, delL = %d" oldA oldB newA newB delL

  let usInst = uniswapInstance us
      usScript = uniswapScript us
      dat = Pool lp $ liquidity + delL
      psC = poolStateCoin us
      lC = mkCoin (liquidityCurrency us) $ lpTicker lp
      psVal = unitValue psC
      lVal = valueOf lC delL
      val = psVal <> valueOf apCoinA newA <> valueOf apCoinB newB
      redeemer = Redeemer $ PlutusTx.toData Add

      lookups =
        Constraints.typedValidatorLookups usInst
          <> Constraints.otherScript usScript
          <> Constraints.monetaryPolicy (liquidityPolicy us)
          <> Constraints.ownPubKeyHash pkh
          <> Constraints.unspentOutputs (Map.singleton oref o)

      tx =
        Constraints.mustPayToTheScript dat val
          <> Constraints.mustForgeValue lVal
          <> Constraints.mustSpendScriptOutput oref redeemer

  logInfo @String $ printf "val = %s, inVal = %s" (show val) (show inVal)
  logInfo $ show lookups
  logInfo $ show tx

  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx

  logInfo $ "added liquidity to pool: " ++ show lp

swapPreview :: Uniswap -> SwapPreviewParams -> Contract w s Text ((Coin A, Amount A), (Coin B, Amount B), Fee)
swapPreview us SwapPreviewParams {..} = do
  (_, (_, o, _, _)) <- findUniswapFactoryAndPool us sppCoinA sppCoinB sppFee
  let outVal = txOutValue $ txOutTxOut o
  let oldA = amountOf outVal sppCoinA
      oldB = amountOf outVal sppCoinB
  let outB = Amount $ findSwap oldA oldB sppAmount sppFee
  return ((sppCoinA, sppAmount), (sppCoinB, outB), sppFee)

-- | Uses a liquidity pool two swap one sort of coins in the pool against the other.
swap :: Uniswap -> SwapParams -> Contract w s Text ()
swap us SwapParams {..} = do
  unless (spAmount > 0) $ throwError "amount must be positive"
  (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us spCoinA spCoinB spFee
  let outVal = txOutValue $ txOutTxOut o
  let oldA = amountOf outVal spCoinA
      oldB = amountOf outVal spCoinB
  (newA, newB, outB) <- do
    let outB = Amount $ findSwap oldA oldB spAmount spFee
    when (outB == 0) $ throwError "no payout"
    return (oldA + spAmount, oldB - outB, outB)

  pkh <- pubKeyHash <$> ownPubKey

  when ((100 * unAmount outB `div` unAmount spResult) < (100 - spSlippage)) $
    throwError "outB amount doesn't meet the slippage criteria"
  logInfo @String $ printf "oldA = %d, oldB = %d, old product = %d, newA = %d, newB = %d, new product = %d" oldA oldB (unAmount oldA * unAmount oldB) newA newB (unAmount newA * unAmount newB)

  let inst = uniswapInstance us
      val = valueOf spCoinA newA <> valueOf spCoinB newB <> unitValue (poolStateCoin us)

      lookups =
        Constraints.typedValidatorLookups inst
          <> Constraints.otherScript (Scripts.validatorScript inst)
          <> Constraints.unspentOutputs (Map.singleton oref o)
          <> Constraints.ownPubKeyHash pkh

      tx =
        Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData Swap)
          <> Constraints.mustPayToTheScript (Pool lp liquidity) val

  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx
  logInfo $ "swapped with: " ++ show lp

indirectSwapPreview :: Uniswap -> ISwapPreviewParams -> Contract w s Text ((Coin A, Amount A), (Coin B, Amount B))
indirectSwapPreview us ISwapPreviewParams {..} = do
  abstractPools <- uniquePools us
  (_, outB) <- case findBestSwap abstractPools (unCoin isppCoinA, unCoin isppCoinB) (unAmount isppAmount) of
    Nothing -> throwError "No path found"
    Just p  -> return p
  return ((isppCoinA, isppAmount), (isppCoinB, Amount outB))

uniquePools :: Uniswap -> Contract w s Text (Map.Map (AssetClass, AssetClass, Fee) (Integer, Integer))
uniquePools us = do
  (_, _, allPools) <- findUniswapFactory us
  relevantPools <- mapM (\lp -> findUniswapPool us lp >>= \x -> pure (x, lp)) allPools
  let uniquePools' =
        Map.fromList $
          distinctPools $
            map
              ( \((_, pO, _), LiquidityPool {..}) ->
                  let pOutVal = txOutValue $ txOutTxOut pO
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

indirectSwap :: Uniswap -> IndirectSwapParams -> Contract w s Text ()
indirectSwap us IndirectSwapParams {..} = do
  unless (ispAmount > 0) $ throwError "amount must be positive"
  (_, _, allPools) <- findUniswapFactory us
  relevantPools <- mapM (\lp -> findUniswapPool us lp >>= \x -> pure (x, lp)) allPools
  abstractPools <- uniquePools us
  let orefs = Map.fromList $ map (\((pRef, pO, _), LiquidityPool {..}) -> ((unCoin lpCoinA, unCoin lpCoinB, lpFee), (pRef, pO))) relevantPools
  let myPools = Map.fromList $ map (\((_, _, liquidity), pool@LiquidityPool {..}) -> ((unCoin lpCoinA, unCoin lpCoinB, lpFee), (pool, liquidity))) relevantPools

  (path, outB) <- case findBestSwap abstractPools (unCoin ispCoinA, unCoin ispCoinB) (unAmount ispAmount) of
    Nothing -> throwError "No path found"
    Just p  -> return p

  when ((100 * outB `div` unAmount ispResult) < (100 - ispSlippage)) $
    throwError "outB amount doesn't meet the slippage criteria"

  logInfo @String (show path)
  let moneyToPay = zip path $ scanl (\amount (a, b, fee) -> let Just (a', b') = Map.lookup (a, b, fee) abstractPools <|> ((\(x, y) -> (y, x)) <$> Map.lookup (b, a, fee) abstractPools) in (Amount $ findSwap (Amount a') (Amount b') amount fee)) ispAmount path
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
                 in Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData Swap)
                      <> Constraints.mustPayToTheScript (Pool pool liquidity) val
            )
            moneyToPay

  --   mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData Swap) <>
  --           Constraints.mustPayToTheScript (Pool lp liquidity) val

  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx

-- | Finds all liquidity pools and their liquidity belonging to the Uniswap instance.
-- This merely inspects the blockchain and does not issue any transactions.
pools :: forall w s. Uniswap -> PoolsParams -> Contract w s Text [((Coin A, Amount A), (Coin B, Amount B), Fee)]
pools us _ = do
  utxos <- utxoAt (uniswapAddress us)
  go $ snd <$> Map.toList utxos
  where
    go :: [TxOutTx] -> Contract w s Text [((Coin A, Amount A), (Coin B, Amount B), Fee)]
    go [] = return []
    go (o : os) = do
      let v = txOutValue $ txOutTxOut o
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
                  s = ((coinA, amtA), (coinB, amtB), lpFee lp)
              logInfo $ "found pool: " ++ show s
              ss <- go os
              return $ s : ss
        else go os
      where
        c :: Coin PoolState
        c = poolStateCoin us

-- | Gets the caller's funds.
funds :: FundsParams -> Contract w s Text Value
funds _ = do
  pkh <- pubKeyHash <$> ownPubKey
  os <- map snd . Map.toList <$> utxoAt (pubKeyHashAddress pkh)
  return $ mconcat [txOutValue $ txOutTxOut o | o <- os]

clearState :: ClearStateParams -> Contract (History (Either Text UserContractState)) s Text ()
clearState ClearStateParams{..} = do
  tell $ WH.remove clsRemoveId

getUniswapDatum :: TxOutTx -> Contract w s Text UniswapDatum
getUniswapDatum o = case txOutDatumHash $ txOutTxOut o of
  Nothing -> throwError "datumHash not found"
  Just h -> case Map.lookup h $ txData $ txOutTxTx o of
    Nothing -> throwError "datum not found"
    Just (Datum e) -> case PlutusTx.fromData e of
      Nothing -> throwError "datum has wrong type"
      Just d  -> return d

findUniswapInstance :: Uniswap -> Coin b -> (UniswapDatum -> Maybe a) -> Contract w s Text (TxOutRef, TxOutTx, a)
findUniswapInstance us c f = do
  let addr = uniswapAddress us
  logInfo @String $ printf "looking for Uniswap instance at address %s containing coin %s " (show addr) (show c)
  utxos <- utxoAt addr
  go [x | x@(_, o) <- Map.toList utxos, isUnity (txOutValue $ txOutTxOut o) c]
  where
    go [] = throwError "Uniswap instance not found"
    go ((oref, o) : xs) = do
      d <- getUniswapDatum o
      case f d of
        Nothing -> go xs
        Just a -> do
          logInfo @String $ printf "found Uniswap instance with datum: %s" (show d)
          return (oref, o, a)

findUniswapFactory :: Uniswap -> Contract w s Text (TxOutRef, TxOutTx, [LiquidityPool])
findUniswapFactory us@Uniswap {..} = findUniswapInstance us usCoin $ \case
  Factory lps -> Just lps
  Pool _ _    -> Nothing

findUniswapPool :: Uniswap -> LiquidityPool -> Contract w s Text (TxOutRef, TxOutTx, Amount Liquidity)
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
    ( (TxOutRef, TxOutTx, [LiquidityPool]),
      (TxOutRef, TxOutTx, LiquidityPool, Amount Liquidity)
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

ownerEndpoint :: Contract (History (Either Text Uniswap)) UniswapOwnerSchema Void ()
ownerEndpoint = startHandler >> ownerEndpoint
  where
    startHandler :: Contract (History (Either Text Uniswap)) UniswapOwnerSchema Void ()
    startHandler = do
      e <- runError (endpoint @"start" >>= \guid -> (guid,) <$> start Nothing)
      case e of
        Left err           -> tell $ WH.append "ERROR" $ Left err
        Right (a,uniswap') -> tell $ WH.append a $ Right uniswap'

ownerEndpoint' :: Contract (History (Either Text Uniswap)) UniswapOwnerSchema' Void ()
ownerEndpoint' = startHandler >> ownerEndpoint'
  where
    startHandler :: Contract (History (Either Text Uniswap)) UniswapOwnerSchema' Void ()
    startHandler = do
      e <- runError (endpoint @"start" >>= \(guid,currency) -> (guid,) <$> start (Just currency))
      case e of
        Left err           -> tell $ WH.append "ERROR" $ Left err
        Right (a,uniswap') -> tell $ WH.append a $ Right uniswap'

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
userEndpoints :: Uniswap -> Contract (History (Either Text UserContractState)) UniswapUserSchema Void ()
userEndpoints us =
  stop
    `select` ( ( f (Proxy @"create") cpOpId (const Created) create
                   `select` f (Proxy @"swap") spOpId (const Swapped) swap
                   `select` f (Proxy @"swapPreview") sppOpId SwapPreview swapPreview
                   `select` f (Proxy @"iSwap") ispOpId (const ISwapped) indirectSwap
                   `select` f (Proxy @"iSwapPreview") isppOpId ISwapPreview indirectSwapPreview
                   `select` f (Proxy @"close") clpOpId (const Closed) close
                   `select` f (Proxy @"remove") rpOpId (const Removed) remove
                   `select` f (Proxy @"add") apOpId (const Added) add
                   `select` f (Proxy @"pools") plOpId Pools pools
                   `select` f (Proxy @"funds") fsOpId Funds (\_us historyId -> funds historyId)
                   `select` f (Proxy @"clearState") clsOpId (const Cleared) (const clearState)
               )
                 >> userEndpoints us
             )
  where
    f ::
      forall l a p.
      (HasEndpoint l p UniswapUserSchema, FromJSON p) =>
      Proxy l ->
      (p -> Text) ->
      (a -> UserContractState) ->
      (Uniswap -> p -> Contract (History (Either Text UserContractState)) UniswapUserSchema Text a) ->
      Contract (History (Either Text UserContractState)) UniswapUserSchema Void ()
    f _ getGuid g c = do

      e <- runError $ do
        p <- endpoint @l
        (getGuid p,) <$> c us p

      case e of
        Left err -> do
          logInfo @Text ("Error during calling endpoint: " <> err)
          tell $ WH.append "ERROR" . Left $ err
        Right (guid,a) ->
          tell $ WH.append guid . Right . g $ a

    stop :: Contract (History (Either Text UserContractState)) UniswapUserSchema Void ()
    stop = do
      e <- runError $ endpoint @"stop"
      tell $ case e of
        Left err               -> WH.append "ERROR" $ Left err
        Right (StopParams{..}) -> WH.append stOpId $ Right Stopped

