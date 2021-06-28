{-# LANGUAGE AllowAmbiguousTypes        #-}
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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Uniswap.OffChain
    ( poolStateCoinFromUniswapCurrency, liquidityCoin
    , CreateParams (..)
    , SwapParams (..)
    , SwapPreviewParams (..)
    , IndirectSwapParams (..)
    , ISwapPreviewParams (..)
    , CloseParams (..)
    , RemoveParams (..)
    , AddParams (..)
    , UniswapUserSchema, UserContractState (..)
    , UniswapOwnerSchema, UniswapOwnerSchema'
    , start, create, add, remove, close, swap, pools
    , ownerEndpoint, ownerEndpoint', userEndpoints
    , uniswap, uniswapTokenName
    ) where
import           Control.Applicative       ((<|>))
import           Control.Monad             hiding (fmap, mapM, mapM_)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Proxy                (Proxy (..))
import           Data.Text                 (Text, pack)
import           Data.Void                 (Void)
import           Debug.Trace
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Playground.Contract
import           Plutus.Contract           hiding (when)
import qualified Plutus.Contracts.Currency as Currency
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup (..), unless)
import           Prelude                   (Semigroup (..), String, div, show)
import           Text.Printf               (printf)
import           Uniswap.IndirectSwaps
import           Uniswap.OnChain           (mkUniswapValidator,
                                            validateLiquidityForging)
import           Uniswap.Pool
import           Uniswap.Types

import           Data.List                 (nubBy, scanl)
data Uniswapping
instance Scripts.ScriptType Uniswapping where
    type instance RedeemerType Uniswapping = UniswapAction
    type instance DatumType    Uniswapping = UniswapDatum

type UniswapOwnerSchema =
    BlockchainActions
        .\/ Endpoint "start" ()

type UniswapOwnerSchema' =
    BlockchainActions
        .\/ Endpoint "start" (CurrencySymbol)




-- | Schema for the endpoints for users of Uniswap.
type UniswapUserSchema =
    BlockchainActions
        .\/ Endpoint "create" CreateParams
        .\/ Endpoint "swap"   SwapParams
        .\/ Endpoint "swapPreview" SwapPreviewParams
        .\/ Endpoint "iSwapPreview" ISwapPreviewParams
        .\/ Endpoint "iSwap"  IndirectSwapParams
        .\/ Endpoint "close"  CloseParams
        .\/ Endpoint "remove" RemoveParams
        .\/ Endpoint "add"    AddParams
        .\/ Endpoint "pools"  ()
        .\/ Endpoint "funds"  ()
        .\/ Endpoint "stop"   ()


-- | Type of the Uniswap user contract state.
data UserContractState =
      Pools [((Coin A, Amount A), (Coin B, Amount B))]
    | Funds Value
    | Created
    | Swapped
    | SwapPreview ((Coin A, Amount A), (Coin B, Amount B))
    | ISwapped
    | ISwapPreview ((Coin A, Amount A), (Coin B, Amount B))
    | Added
    | Removed
    | Closed
    | Stopped
    deriving (Show, Generic, FromJSON, ToJSON)


uniswapTokenName, poolStateTokenName :: TokenName
uniswapTokenName = "Uniswap"
poolStateTokenName = "Pool State"

uniswapInstance :: Uniswap -> Scripts.ScriptInstance Uniswapping
uniswapInstance us = Scripts.validator @Uniswapping
    ($$(PlutusTx.compile [|| mkUniswapValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode us
        `PlutusTx.applyCode` PlutusTx.liftCode c)
     $$(PlutusTx.compile [|| wrap ||])
  where
    c :: Coin PoolState
    c = poolStateCoin us

    wrap = Scripts.wrapValidator @UniswapDatum @UniswapAction

uniswapScript :: Uniswap -> Validator
uniswapScript = Scripts.validatorScript . uniswapInstance

uniswapAddress :: Uniswap -> Ledger.Address
uniswapAddress = Ledger.scriptAddress . uniswapScript

uniswap :: CurrencySymbol -> Uniswap
uniswap cs = Uniswap $ mkCoin cs uniswapTokenName

liquidityPolicy :: Uniswap -> MonetaryPolicy
liquidityPolicy us = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| \u t -> Scripts.wrapMonetaryPolicy (validateLiquidityForging u t) ||])
        `PlutusTx.applyCode` PlutusTx.liftCode us
        `PlutusTx.applyCode` PlutusTx.liftCode poolStateTokenName

liquidityCurrency :: Uniswap -> CurrencySymbol
liquidityCurrency = scriptCurrencySymbol . liquidityPolicy

poolStateCoin :: Uniswap -> Coin PoolState
poolStateCoin = flip mkCoin poolStateTokenName . liquidityCurrency

-- | Gets the 'Coin' used to identity liquidity pools.
poolStateCoinFromUniswapCurrency :: CurrencySymbol -- ^ The currency identifying the Uniswap instance.
                                 -> Coin PoolState
poolStateCoinFromUniswapCurrency = poolStateCoin . uniswap

-- | Gets the liquidity token for a given liquidity pool.
liquidityCoin :: CurrencySymbol -- ^ The currency identifying the Uniswap instance.
              -> Coin A         -- ^ One coin in the liquidity pair.
              -> Coin B         -- ^ The other coin in the liquidity pair.
              -> Coin Liquidity
liquidityCoin cs coinA coinB = mkCoin (liquidityCurrency $ uniswap cs) $ lpTicker $ LiquidityPool coinA coinB

-- | Parameters for the @create@-endpoint, which creates a new liquidity pool.
data CreateParams = CreateParams
    { cpCoinA   :: Coin A   -- ^ One 'Coin' of the liquidity pair.
    , cpCoinB   :: Coin B   -- ^ The other 'Coin'.
    , cpAmountA :: Amount A -- ^ Amount of liquidity for the first 'Coin'.
    , cpAmountB :: Amount B -- ^ Amount of liquidity for the second 'Coin'.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @swap@-endpoint, which allows swaps between the two different coins in a liquidity pool.
-- One of the provided amounts must be positive, the other must be zero.
data SwapParams = SwapParams
    { spCoinA    :: Coin A         -- ^ One 'Coin' of the liquidity pair.
    , spCoinB    :: Coin B         -- ^ The other 'Coin'.
    , spAmount   :: Amount A       -- ^ The amount the first 'Coin' that should be swapped.
    , spResult   :: Amount B
    , spSlippage :: Integer
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data SwapPreviewParams = SwapPreviewParams
    { sppCoinA  :: Coin A
    , sppCoinB  :: Coin B
    , sppAmount :: Amount A
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data IndirectSwapParams = IndirectSwapParams
    { ispCoinA    :: Coin A         -- ^ One 'Coin' of the liquidity pair.
    , ispCoinB    :: Coin B         -- ^ The other 'Coin'.
    , ispAmount   :: Amount A       -- ^ The amount the first 'Coin' that should be swapped.
    , ispResult   :: Amount B
    , ispSlippage :: Integer
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data ISwapPreviewParams = ISwapPreviewParams
    { isppCoinA  :: Coin A
    , isppCoinB  :: Coin B
    , isppAmount :: Amount A
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)


-- | Parameters for the @close@-endpoint, which closes a liquidity pool.
data CloseParams = CloseParams
    { clpCoinA :: Coin A         -- ^ One 'Coin' of the liquidity pair.
    , clpCoinB :: Coin B         -- ^ The other 'Coin' of the liquidity pair.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @remove@-endpoint, which removes some liquidity from a liquidity pool.
data RemoveParams = RemoveParams
    { rpCoinA :: Coin A           -- ^ One 'Coin' of the liquidity pair.
    , rpCoinB :: Coin B           -- ^ The other 'Coin' of the liquidity pair.
    , rpDiff  :: Amount Liquidity -- ^ The amount of liquidity tokens to burn in exchange for liquidity from the pool.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @add@-endpoint, which adds liquidity to a liquidity pool in exchange for liquidity tokens.
data AddParams = AddParams
    { apCoinA   :: Coin A         -- ^ One 'Coin' of the liquidity pair.
    , apCoinB   :: Coin B         -- ^ The other 'Coin' of the liquidity pair.
    , apAmountA :: Amount A       -- ^ The amount of coins of the first kind to add to the pool.
    , apAmountB :: Amount B       -- ^ The amount of coins of the second kind to add to the pool.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Creates a Uniswap "factory". This factory will keep track of the existing liquidity pools and enforce that there will be at most one liquidity pool
-- for any pair of tokens at any given time.
start :: HasBlockchainActions s => Maybe CurrencySymbol -> Contract w s Text Uniswap
start mcs = do
    pkh <- pubKeyHash <$> ownPubKey


    cs  <- case mcs of
        Nothing -> fmap Currency.currencySymbol
                        $ mapError (pack . show @Currency.CurrencyError)
                        $ Currency.forgeContract pkh [(uniswapTokenName, 1)]
        Just jcs -> return jcs
    let c    = mkCoin cs uniswapTokenName
        us   = uniswap cs
        inst = uniswapInstance us
        tx   = mustPayToTheScript (Factory []) $ unitValue c
    ledgerTx <- submitTxConstraints inst tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo @String $ printf "started Uniswap %s at address %s" (show us) (show $ uniswapAddress us)
    return us

-- | Creates a liquidity pool for a pair of coins. The creator provides liquidity for both coins and gets liquidity tokens in return.
create :: HasBlockchainActions s => Uniswap -> CreateParams -> Contract w s Text ()
create us CreateParams{..} = do
    when (unCoin cpCoinA == unCoin cpCoinB) $ throwError "coins must be different"
    when (cpAmountA <= 0 || cpAmountB <= 0) $ throwError "amounts must be positive"
    (oref, o, lps) <- findUniswapFactory us
    let liquidity = calculateInitialLiquidity cpAmountA cpAmountB
        lp        = LiquidityPool {lpCoinA = cpCoinA, lpCoinB = cpCoinB}
    let usInst   = uniswapInstance us
        usScript = uniswapScript us
        usDat1   = Factory $ lp : lps
        usDat2   = Pool lp liquidity
        psC      = poolStateCoin us
        lC       = mkCoin (liquidityCurrency us) $ lpTicker lp
        usVal    = unitValue $ usCoin us
        lpVal    = valueOf cpCoinA cpAmountA <> valueOf cpCoinB cpAmountB <> unitValue psC

        lookups  = Constraints.scriptInstanceLookups usInst        <>
                   Constraints.otherScript usScript                <>
                   Constraints.monetaryPolicy (liquidityPolicy us) <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScript usDat1 usVal                                     <>
                   Constraints.mustPayToTheScript usDat2 lpVal                                     <>
                   Constraints.mustForgeValue (unitValue psC <> valueOf lC liquidity)              <>
                   Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData $ Create lp)

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "created liquidity pool: " ++ show lp

-- | Closes a liquidity pool by burning all remaining liquidity tokens in exchange for all liquidity remaining in the pool.
close :: HasBlockchainActions s => Uniswap -> CloseParams -> Contract w s Text ()
close us CloseParams{..} = do
    ((oref1, o1, lps), (oref2, o2, lp, liquidity)) <- findUniswapFactoryAndPool us clpCoinA clpCoinB
    pkh                                            <- pubKeyHash <$> ownPubKey
    let usInst   = uniswapInstance us
        usScript = uniswapScript us
        usDat    = Factory $ filter (/= lp) lps
        usC      = usCoin us
        psC      = poolStateCoin us
        lC       = mkCoin (liquidityCurrency us) $ lpTicker lp
        usVal    = unitValue usC
        psVal    = unitValue psC
        lVal     = valueOf lC liquidity
        redeemer = Redeemer $ PlutusTx.toData Close

        lookups  = Constraints.scriptInstanceLookups usInst        <>
                   Constraints.otherScript usScript                <>
                   Constraints.monetaryPolicy (liquidityPolicy us) <>
                   Constraints.ownPubKeyHash pkh                   <>
                   Constraints.unspentOutputs (Map.singleton oref1 o1 <> Map.singleton oref2 o2)

        tx       = Constraints.mustPayToTheScript usDat usVal          <>
                   Constraints.mustForgeValue (negate $ psVal <> lVal) <>
                   Constraints.mustSpendScriptOutput oref1 redeemer    <>
                   Constraints.mustSpendScriptOutput oref2 redeemer    <>
                   Constraints.mustIncludeDatum (Datum $ PlutusTx.toData $ Pool lp liquidity)

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "closed liquidity pool: " ++ show lp

-- | Removes some liquidity from a liquidity pool in exchange for liquidity tokens.
remove :: HasBlockchainActions s => Uniswap -> RemoveParams -> Contract w s Text ()
remove us RemoveParams{..} = do
    (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us rpCoinA rpCoinB
    pkh                           <- pubKeyHash <$> ownPubKey
    when (rpDiff < 1 || rpDiff >= liquidity) $ throwError "removed liquidity must be positive and less than total liquidity"
    let usInst       = uniswapInstance us
        usScript     = uniswapScript us
        dat          = Pool lp $ liquidity - rpDiff
        psC          = poolStateCoin us
        lC           = mkCoin (liquidityCurrency us) $ lpTicker lp
        psVal        = unitValue psC
        lVal         = valueOf lC rpDiff
        inVal        = txOutValue $ txOutTxOut o
        inA          = amountOf inVal rpCoinA
        inB          = amountOf inVal rpCoinB
        (outA, outB) = calculateRemoval inA inB liquidity rpDiff
        val          = psVal <> valueOf rpCoinA outA <> valueOf rpCoinB outB
        redeemer     = Redeemer $ PlutusTx.toData Remove

        lookups  = Constraints.scriptInstanceLookups usInst          <>
                   Constraints.otherScript usScript                  <>
                   Constraints.monetaryPolicy (liquidityPolicy us)   <>
                   Constraints.unspentOutputs (Map.singleton oref o) <>
                   Constraints.ownPubKeyHash pkh

        tx       = Constraints.mustPayToTheScript dat val          <>
                   Constraints.mustForgeValue (negate lVal)        <>
                   Constraints.mustSpendScriptOutput oref redeemer

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "removed liquidity from pool: " ++ show lp

-- | Adds some liquidity to an existing liquidity pool in exchange for newly minted liquidity tokens.
add :: HasBlockchainActions s => Uniswap -> AddParams -> Contract w s Text ()
add us AddParams{..} = do
    pkh                           <- pubKeyHash <$> ownPubKey
    (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us apCoinA apCoinB
    logInfo @String $ printf "old liquidity = %d" $ unAmount liquidity
    when (apAmountA < 0 || apAmountB < 0) $ throwError "amounts must not be negative"
    let outVal = txOutValue $ txOutTxOut o
        oldA   = amountOf outVal apCoinA
        oldB   = amountOf outVal apCoinB
        newA   = oldA + apAmountA
        newB   = oldB + apAmountB
        delL   = calculateAdditionalLiquidity oldA oldB liquidity apAmountA apAmountB
        inVal  = valueOf apCoinA apAmountA <> valueOf apCoinB apAmountB
    when (delL <= 0) $ throwError "insufficient liquidity"
    logInfo @String $ printf "oldA = %d, oldB = %d, newA = %d, newB = %d, delL = %d" oldA oldB newA newB delL

    let usInst       = uniswapInstance us
        usScript     = uniswapScript us
        dat          = Pool lp $ liquidity + delL
        psC          = poolStateCoin us
        lC           = mkCoin (liquidityCurrency us) $ lpTicker lp
        psVal        = unitValue psC
        lVal         = valueOf lC delL
        val          = psVal <> valueOf apCoinA newA <> valueOf apCoinB newB
        redeemer     = Redeemer $ PlutusTx.toData Add

        lookups  = Constraints.scriptInstanceLookups usInst             <>
                   Constraints.otherScript usScript                     <>
                   Constraints.monetaryPolicy (liquidityPolicy us)      <>
                   Constraints.ownPubKeyHash pkh                        <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScript dat val          <>
                   Constraints.mustForgeValue lVal                 <>
                   Constraints.mustSpendScriptOutput oref redeemer

    logInfo @String $ printf "val = %s, inVal = %s" (show val) (show inVal)
    logInfo $ show lookups
    logInfo $ show tx

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "added liquidity to pool: " ++ show lp





swapPreview :: HasBlockchainActions s => Uniswap -> SwapPreviewParams ->  Contract w s Text ((Coin A, Amount A),(Coin B, Amount B))
swapPreview us SwapPreviewParams{..} = do
    (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us sppCoinA sppCoinB
    let outVal = txOutValue $ txOutTxOut o
    let oldA = amountOf outVal sppCoinA
        oldB = amountOf outVal sppCoinB
    let outB = Amount $ findSwap oldA oldB sppAmount
    return ((sppCoinA, sppAmount),(sppCoinB, outB))


-- | Uses a liquidity pool two swap one sort of coins in the pool against the other.
swap :: HasBlockchainActions s => Uniswap -> SwapParams -> Contract w s Text ()
swap us SwapParams{..} = do
    unless (spAmount > 0) $ throwError "amount must be positive"
    (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us spCoinA spCoinB
    let outVal = txOutValue $ txOutTxOut o
    let oldA = amountOf outVal spCoinA
        oldB = amountOf outVal spCoinB
    (newA, newB, outB) <- do
        let outB = Amount $ findSwap oldA oldB spAmount
        when (outB == 0) $ throwError "no payout"
        return (oldA + spAmount, oldB - outB, outB)

    pkh <- pubKeyHash <$> ownPubKey

    when ((100*unAmount outB `div` unAmount spResult) < (100 - spSlippage)) $
      throwError "outB amount doesn't meet the slippage criteria"
    logInfo @String $ printf "oldA = %d, oldB = %d, old product = %d, newA = %d, newB = %d, new product = %d" oldA oldB (unAmount oldA * unAmount oldB) newA newB (unAmount newA * unAmount newB)

    let inst    = uniswapInstance us
        val     = valueOf spCoinA newA <> valueOf spCoinB newB <> unitValue (poolStateCoin us)

        lookups = Constraints.scriptInstanceLookups inst                 <>
                  Constraints.otherScript (Scripts.validatorScript inst) <>
                  Constraints.unspentOutputs (Map.singleton oref o)      <>
                  Constraints.ownPubKeyHash pkh

        tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData Swap) <>
                  Constraints.mustPayToTheScript (Pool lp liquidity) val

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo $ "swapped with: " ++ show lp



indirectSwapPreview :: HasBlockchainActions s => Uniswap -> ISwapPreviewParams -> Contract w s Text ((Coin A, Amount A), (Coin B, Amount B))
indirectSwapPreview us ISwapPreviewParams{..} = do
    (oref, o, allPools) <- findUniswapFactory us
    let outVal = txOutValue $ txOutTxOut o

    relevantPools <- mapM (\lp -> findUniswapPool us lp >>= \x -> pure (x,lp)) allPools

    let abstractPools = Map.fromList $ nubBy (\((a1,a2),_) ((b1,b2),_) -> (a1,a2)==(b1,b2) || (a1,a2)==(b2,b1)) $ (map (\((pRef, pO,_), LiquidityPool {..}) ->
            let pOutVal = txOutValue $ txOutTxOut pO
            in ((unCoin lpCoinA, unCoin lpCoinB), (unAmount $ amountOf pOutVal lpCoinA, unAmount $ amountOf pOutVal lpCoinB)))) relevantPools

    let orefs = Map.fromList $ nubBy (\a b -> fst a == fst b) $ map (\((pRef, pO,_), LiquidityPool {..}) -> ((unCoin lpCoinA, unCoin lpCoinB), (pRef, pO))) relevantPools
    let pools = Map.fromList $ nubBy (\a b -> fst a == fst b) $ map (\((_,_,liquidity), pool@LiquidityPool{..}) -> ((unCoin lpCoinA, unCoin lpCoinB), (pool,liquidity))) relevantPools
    (path,outB) <- case findBestSwap abstractPools (unCoin isppCoinA,unCoin isppCoinB) (unAmount isppAmount) of
        Nothing -> throwError "No path found"
        Just p  -> return p
    return ((isppCoinA, isppAmount), (isppCoinB, Amount outB))

indirectSwap :: HasBlockchainActions s => Uniswap -> IndirectSwapParams -> Contract w s Text ()
indirectSwap us IndirectSwapParams{..} = do
    unless (ispAmount > 0) $ throwError "amount must be positive"
    (oref, o, allPools) <- findUniswapFactory us
    let outVal = txOutValue $ txOutTxOut o

    relevantPools <- mapM (\lp -> findUniswapPool us lp >>= \x -> pure (x,lp)) allPools

    let abstractPools = Map.fromList $ nubBy (\((a1,a2),_) ((b1,b2),_) -> (a1,a2)==(b1,b2) || (a1,a2)==(b2,b1)) $ (map (\((pRef, pO,_), LiquidityPool {..}) ->
            let pOutVal = txOutValue $ txOutTxOut pO
            in ((unCoin lpCoinA, unCoin lpCoinB), (unAmount $ amountOf pOutVal lpCoinA, unAmount $ amountOf pOutVal lpCoinB)))) relevantPools

    let orefs = Map.fromList $ nubBy (\a b -> fst a == fst b) $ map (\((pRef, pO,_), LiquidityPool {..}) -> ((unCoin lpCoinA, unCoin lpCoinB), (pRef, pO))) relevantPools
    let pools = Map.fromList $ nubBy (\a b -> fst a == fst b) $ map (\((_,_,liquidity), pool@LiquidityPool{..}) -> ((unCoin lpCoinA, unCoin lpCoinB), (pool,liquidity))) relevantPools
    (path,outB) <- case findBestSwap abstractPools (unCoin ispCoinA,unCoin ispCoinB) (unAmount ispAmount) of
        Nothing -> throwError "No path found"
        Just p  -> return p

    when ((100* outB `div` unAmount ispResult) < (100 - ispSlippage)) $
      throwError "outB amount doesn't meet the slippage criteria"


    logInfo @String (show path)
    let moneyToPay = zip path $ scanl (\amount (a,b) -> let Just (a',b') = Map.lookup (a,b) abstractPools <|> ((\(x,y)->(y,x)) <$> Map.lookup (b,a) abstractPools) in (Amount $ findSwap (Amount a') (Amount b') amount)) ispAmount path
    pkh <- pubKeyHash <$> ownPubKey

    let inst    = uniswapInstance us

        lookups = Constraints.scriptInstanceLookups inst                 <>
                  Constraints.otherScript (Scripts.validatorScript inst) <>
                  Constraints.unspentOutputs (Map.fromList $ map (\((oref,o,_),_) -> (oref,o)) relevantPools)      <>
                  Constraints.ownPubKeyHash pkh

        tx      = mconcat $ map (\((a,b),amount) ->
                                    let Just (oref,_) = Map.lookup (a,b) orefs <|> Map.lookup (b,a) orefs
                                        Just (pool, liquidity) = Map.lookup (a,b) pools <|> Map.lookup (b,a) pools
                                        Just (oldA,oldB) = Map.lookup (a,b) abstractPools <|> (let Just (b',a') = Map.lookup (b,a) abstractPools in Just (a',b'))
                                        (newA,newB) = (oldA+ unAmount amount, oldB - findSwap (Amount oldA) (Amount oldB) amount)
                                        val = valueOf (Coin a) (Amount newA) <> valueOf (Coin b) (Amount newB) <> unitValue (poolStateCoin us)
                                    in Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData Swap) <>
                                       Constraints.mustPayToTheScript (Pool pool liquidity) val

                 ) moneyToPay

        --   mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData Swap) <>
        --           Constraints.mustPayToTheScript (Pool lp liquidity) val

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx







-- | Finds all liquidity pools and their liquidity belonging to the Uniswap instance.
-- This merely inspects the blockchain and does not issue any transactions.
pools :: forall w s. HasBlockchainActions s => Uniswap -> Contract w s Text [((Coin A, Amount A), (Coin B, Amount B))]
pools us = do
    utxos <- utxoAt (uniswapAddress us)
    go $ snd <$> Map.toList utxos
  where
    go :: [TxOutTx] -> Contract w s Text [((Coin A, Amount A), (Coin B, Amount B))]
    go []       = return []
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
                            amtA  = amountOf v coinA
                            amtB  = amountOf v coinB
                            s     = ((coinA, amtA), (coinB, amtB))
                        logInfo $ "found pool: " ++ show s
                        ss <- go os
                        return $ s : ss
            else go os
      where
        c :: Coin PoolState
        c = poolStateCoin us

-- | Gets the caller's funds.
funds :: HasBlockchainActions s => Contract w s Text Value
funds = do
    pkh <- pubKeyHash <$> ownPubKey
    os  <- map snd . Map.toList <$> utxoAt (pubKeyHashAddress pkh)
    return $ mconcat [txOutValue $ txOutTxOut o | o <- os]

getUniswapDatum :: TxOutTx -> Contract w s Text UniswapDatum
getUniswapDatum o = case txOutDatumHash $ txOutTxOut o of
        Nothing -> throwError "datumHash not found"
        Just h -> case Map.lookup h $ txData $ txOutTxTx o of
            Nothing -> throwError "datum not found"
            Just (Datum e) -> case PlutusTx.fromData e of
                Nothing -> throwError "datum has wrong type"
                Just d  -> return d

findUniswapInstance :: HasBlockchainActions s => Uniswap -> Coin b -> (UniswapDatum -> Maybe a) -> Contract w s Text (TxOutRef, TxOutTx, a)
findUniswapInstance us c f = do
    let addr = uniswapAddress us
    logInfo @String $ printf "looking for Uniswap instance at address %s containing coin %s " (show addr) (show c)
    utxos <- utxoAt addr
    go  [x | x@(_, o) <- Map.toList utxos, isUnity (txOutValue $ txOutTxOut o) c]
  where
    go [] = throwError "Uniswap instance not found"
    go ((oref, o) : xs) = do
        d <- getUniswapDatum o
        case f d of
            Nothing -> go xs
            Just a  -> do
                logInfo @String $ printf "found Uniswap instance with datum: %s" (show d)
                return (oref, o, a)

findUniswapFactory :: HasBlockchainActions s => Uniswap -> Contract w s Text (TxOutRef, TxOutTx, [LiquidityPool])
findUniswapFactory us@Uniswap{..} = findUniswapInstance us usCoin $ \case
    Factory lps -> Just lps
    Pool _ _    -> Nothing

findUniswapPool :: HasBlockchainActions s => Uniswap -> LiquidityPool -> Contract w s Text (TxOutRef, TxOutTx, Amount Liquidity)
findUniswapPool us lp = findUniswapInstance us (poolStateCoin us) $ \case
        Pool lp' l
            | lp == lp' -> Just l
        _               -> Nothing

findUniswapFactoryAndPool :: HasBlockchainActions s
                          => Uniswap
                          -> Coin A
                          -> Coin B
                          -> Contract w s Text ( (TxOutRef, TxOutTx, [LiquidityPool])
                                               , (TxOutRef, TxOutTx, LiquidityPool, Amount Liquidity)
                                               )
findUniswapFactoryAndPool us coinA coinB = do
    (oref1, o1, lps) <- findUniswapFactory us
    case [ lp'
         | lp' <- lps
         , lp' == LiquidityPool coinA coinB
         ] of
        [lp] -> do
            (oref2, o2, a) <- findUniswapPool us lp
            return ( (oref1, o1, lps)
                   , (oref2, o2, lp, a)
                   )
        _    -> throwError "liquidity pool not found"


ownerEndpoint :: Contract (Last (Either Text Uniswap)) UniswapOwnerSchema Void ()
ownerEndpoint = (startHandler) >> ownerEndpoint
    where

      startHandler :: Contract (Last (Either Text Uniswap)) UniswapOwnerSchema Void ()
      startHandler = do
           e <- runError $ (endpoint @"start" >> start Nothing)
           tell $ Last $ Just e

ownerEndpoint' :: Contract (Last (Either Text Uniswap)) UniswapOwnerSchema' Void ()
ownerEndpoint' = (startHandler) >> ownerEndpoint'
    where
      startHandler :: Contract (Last (Either Text Uniswap)) UniswapOwnerSchema' Void ()
      startHandler = do
           e <- runError $ (endpoint @"start" >>= start . Just)
           tell $ Last $ Just e

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
userEndpoints :: Uniswap -> Contract (Last (Either Text UserContractState)) UniswapUserSchema Void ()
userEndpoints us =
    stop
        `select`
    ((f @"create" (const Created) create                 `select`
      f @"swap"   (const Swapped) swap                   `select`
      f @"swapPreview" SwapPreview swapPreview                 `select`
      f @"iSwap"  (const ISwapped) indirectSwap          `select`
      f @"iSwapPreview" ISwapPreview indirectSwapPreview                 `select`
      f @"close"  (const Closed)  close                  `select`
      f @"remove" (const Removed) remove                 `select`
      f @"add"    (const Added)   add                    `select`
      f @"pools"  Pools           (\us' () -> pools us') `select`
      f @"funds"  Funds           (\_us () -> funds))    >> userEndpoints us)
  where
    f :: forall l a p.
         HasEndpoint l p UniswapUserSchema
      => (a -> UserContractState)
      -> (Uniswap -> p -> Contract (Last (Either Text UserContractState)) UniswapUserSchema Text a)
      -> Contract (Last (Either Text UserContractState)) UniswapUserSchema Void ()
    f g c = do
        e <- runError $ do
            p <- endpoint @l
            c us p

        case e of
            Left err -> logInfo @Text ("ERROR POLECIAL ERROR:      " <> err) >> (tell $ Last $ Just $ Left err)
            Right a  -> tell $ Last $ Just $ Right $ g a

    stop :: Contract (Last (Either Text UserContractState)) UniswapUserSchema Void ()
    stop = do
        e <- runError $ endpoint @"stop"
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right () -> Right Stopped

