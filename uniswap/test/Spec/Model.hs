{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
module Spec.Model
      where

import           Control.Applicative                ((<|>))
import           Control.Lens                       hiding (elements)
import           Control.Monad
import           Control.Monad                      (void, when)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (isJust, isNothing)
import           Data.Monoid                        (Last (..))
import           Data.String                        (IsString (..))
import           Data.Text                          (Text, pack)
import           Data.Void
import           Ledger                             hiding (singleton, to)
import           Ledger.Ada                         as Ada
import           Ledger.Value                       hiding (valueOf)
import           Plutus.Contract                    (Contract, logInfo,
                                                     mapError, runError)
import           Plutus.Contract.Test
import           Plutus.Contract.Test.ContractModel
import qualified Plutus.Contracts.Currency          as Currency
import           Plutus.Trace.Emulator
import qualified Plutus.V1.Ledger.Value             as Value
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Uniswap.IndirectSwaps
import           Uniswap.OffChain                   hiding (uniswap)
import qualified Uniswap.OffChain
import           Uniswap.Pool
import           Uniswap.Types






type PoolData = ((Coin A, Integer),(Coin B, Integer),Integer)




data UModel = UModel { _modelPools :: Map LiquidityPool PoolData
                        , _uniswap :: Maybe Uniswap}
    deriving Show

makeLenses ''UModel

emptyUModel = UModel Map.empty Nothing


tests :: TestTree
tests = testProperty "uniswap model" prop_U



amountForCoin pool@LiquidityPool{..} coin = modelPools . at pool . _Just . getAmount
    where getAmount = lens get set
          get ((ca,a),(_,b),_) | unCoin ca == unCoin coin = a
                               | otherwise                = b

          set ((ca,a),(cb,b),l) v | unCoin ca == unCoin coin = ((ca,v),(cb,b),l)
                                  | otherwise                = ((ca,a),(cb,v),l)



getAmountForCoin' :: ModelState UModel -> LiquidityPool -> Coin c -> Maybe Integer
getAmountForCoin' s pool coin = s ^? contractState . amountForCoin pool coin

getAmountForCoin :: LiquidityPool -> Coin c -> Spec UModel (Maybe Integer)
getAmountForCoin pool coin = do
    s <- getModelState
    return $ getAmountForCoin' s pool coin

mkPool :: (Coin A, Coin B) -> Integer -> Integer -> Integer -> PoolData
mkPool (ca,cb) a b l = ((ca,fromIntegral a),(cb,fromIntegral b),l)

liquidity pool = modelPools . ix pool . _3


getLiquidity' :: ModelState UModel -> LiquidityPool -> Maybe Integer
getLiquidity' s pool = s ^? contractState . liquidity pool

getLiquidity :: LiquidityPool -> Spec UModel (Maybe Integer)
getLiquidity pool = do
    s <- getModelState
    return $ getLiquidity' s pool

getPoolState' :: ModelState UModel -> LiquidityPool -> Maybe PoolData
getPoolState' s pool@LiquidityPool{..} = (s ^. contractState . modelPools . at pool)


sortPool LiquidityPool{..} = LiquidityPool (Coin newA) (Coin newB)
    where newA = min (unCoin lpCoinA) (unCoin lpCoinB)
          newB = max (unCoin lpCoinA) (unCoin lpCoinB)

getPoolState :: LiquidityPool -> Spec UModel (Maybe PoolData)
getPoolState pool = do
    s <- getModelState
    return $ getPoolState' s pool


getPools' :: ModelState UModel -> [((Coin A, Integer),(Coin B, Integer))]
getPools' s = map (\(a,b,_) -> (a,b)) (s ^.. contractState . modelPools . traversed)

getPools :: Spec UModel [((Coin A, Integer),(Coin B, Integer))]
getPools = do
    s <- getModelState
    return $ getPools' s

getUniswap' :: ModelState UModel -> Maybe Uniswap
getUniswap' s = s ^. contractState . uniswap

getUniswap :: Spec UModel (Maybe Uniswap)
getUniswap = do
    s <- getModelState
    return $ getUniswap' s





instance ContractModel UModel where

    data Action UModel =
              StartA Wallet
            | CreateA Wallet (Coin A, Coin B) Integer Integer
            | AddA Wallet (Coin A, Coin B) Integer Integer
            | RemoveA Wallet (Coin A, Coin B) Integer
            | CloseA Wallet (Coin A, Coin B)
            | SwapA Wallet (Coin A, Coin B) Integer
            | ISwapA Wallet (Coin A, Coin B) Integer
        deriving (Show, Eq)

    data ContractInstanceKey UModel w s e where
        StartKey :: Wallet           -> ContractInstanceKey UModel (Last (Either Text Uniswap)) UniswapOwnerSchema' Void
        UseKey   :: Wallet           -> ContractInstanceKey UModel (Last (Either Text UserContractState)) UniswapUserSchema    Void

    instanceTag key _ = fromString $ "instance tag for: " ++ show key

    arbitraryAction _ = oneof $
        (StartA <$> (pure $ Wallet 1)) :
        [ CreateA  <$> genWallet <*> genCoinPair <*> genNonNeg <*> genNonNeg ]               ++
        [ AddA <$> genWallet <*> genCoinPair <*> genNonNeg <*> genNonNeg ]               ++
        [ RemoveA <$> genWallet <*> genCoinPair <*> genNonNeg ]               ++
        [ CloseA  <$> genWallet <*> genCoinPair] ++
        [ SwapA <$> genWallet <*> genCoinPair <*> genNonNeg] ++
        [ ISwapA <$> genWallet <*> genCoinPair <*> genNonNeg]

    initialState = emptyUModel

    nextState (StartA w) = do
        withdraw w nft
        uniswap $= Just (Uniswap.OffChain.uniswap uniswapCurrencySymbol)
        wait 1




    nextState (CreateA w (coinA,coinB) amountA amountB) = do
        let pool@LiquidityPool{..} = liquidityPool (coinA,coinB)
        bc <- askModelState $ view $ balanceChange w
        mUniswap <- getUniswap
        mPool <- getPoolState pool
        case (mUniswap, mPool) of
            (Just us, Nothing)
                    | tokenAmt + assetClassValueOf bc (unCoin coinA) >= amountA &&
                      tokenAmt + assetClassValueOf bc (unCoin coinB) >= amountB
                -> do
                    let liquidityAmount = calculateInitialLiquidity (Amount amountA) (Amount amountB)
                    let lC = liquidityCoin (fst $ unAssetClass $ unCoin $ usCoin us) lpCoinA lpCoinB
                    (modelPools . at pool) $= Just (mkPool (coinA, coinB) amountA amountB (unAmount liquidityAmount))
                    withdraw w (valueOf coinA (Amount amountA) <> valueOf coinB (Amount amountB))
                    deposit w (Uniswap.Types.valueOf lC liquidityAmount)
            _ -> return ()
        wait 1

    nextState (AddA w (coinA,coinB) amountA amountB) = do
        let pool@LiquidityPool{..} = liquidityPool (coinA,coinB)
        bc <- askModelState $ view $ balanceChange w
        mUniswap <- getUniswap
        mOldA <- getAmountForCoin pool coinA
        mOldB <- getAmountForCoin pool coinB
        mLiquidity <- getLiquidity pool
        case (mUniswap, liftM3 (,,) mOldA mOldB mLiquidity) of
            (Just us, Just (oldA, oldB, oldLiquidity))
                               | tokenAmt + assetClassValueOf bc (unCoin coinA) >= amountA &&
                                 tokenAmt + assetClassValueOf bc (unCoin coinB) >= amountB
                -> do

                    let delLiquidity = calculateAdditionalLiquidity (Amount oldA) (Amount oldB) (Amount oldLiquidity) (Amount amountA) (Amount amountB)
                    let lC = liquidityCoin (fst $ unAssetClass $ unCoin $ usCoin us) lpCoinA lpCoinB

                    liquidity pool $= (oldLiquidity+ fromIntegral delLiquidity)
                    amountForCoin pool coinA $= (oldA+amountA)
                    amountForCoin pool coinB $= (oldB+amountB)
                    withdraw w (valueOf coinA (Amount amountA) <> valueOf coinB (Amount amountB))
                    deposit w (Uniswap.Types.valueOf lC delLiquidity)
            _ -> return ()
        wait 1

    nextState (RemoveA w (coinA,coinB) rpDiff) = do
        let pool@LiquidityPool{..} = liquidityPool (coinA,coinB)
        bc <- askModelState $ view $ balanceChange w
        mUniswap <- getUniswap
        mOldA <- getAmountForCoin pool coinA
        mOldB <- getAmountForCoin pool coinB
        mLiquidity <- getLiquidity pool
        case (mUniswap, liftM3 (,,) mOldA mOldB mLiquidity) of
            (Just us, Just (oldA, oldB, oldLiquidity))
                               | rpDiff <= oldLiquidity
                -> do
                    let lC = liquidityCoin (fst $ unAssetClass $ unCoin $ usCoin us) lpCoinA lpCoinB
                    when (rpDiff <= assetClassValueOf bc (unCoin lC) && rpDiff < oldLiquidity) $ do
                        let (Amount outA, Amount outB) = calculateRemoval (Amount oldA) (Amount oldB) (Amount oldLiquidity) (Amount rpDiff)

                        liquidity pool $= (oldLiquidity - rpDiff)
                        amountForCoin pool coinA $= outA
                        amountForCoin pool coinB $= outB

                        deposit w (valueOf coinA (Amount (oldA - outA)) <> valueOf coinB (Amount (oldB - outB)))
                        withdraw w (Uniswap.Types.valueOf lC (Amount rpDiff))
            _ -> return ()
        wait 1

    nextState (CloseA w (coinA,coinB)) = do
        let pool = liquidityPool (coinA,coinB)
        bc <- askModelState $ view $ balanceChange w
        mUniswap <- getUniswap
        mOldA <- getAmountForCoin pool coinA
        mOldB <- getAmountForCoin pool coinB
        mLiquidity <- getLiquidity pool
        case (mUniswap, liftM3 (,,) mOldA mOldB mLiquidity) of
            (Just us, Just (oldA, oldB, oldLiquidity))
                -> do
                    let lC = liquidityCoin (fst $ unAssetClass $ unCoin $ usCoin us) coinA coinB
                    when (oldLiquidity == assetClassValueOf bc (unCoin lC)) $ do
                        (modelPools . at pool) $= Nothing
                        deposit w (valueOf coinA (Amount oldA) <> valueOf coinB (Amount oldB))
                        withdraw w (Uniswap.Types.valueOf lC (Amount oldLiquidity))
            _ -> return ()
        wait 1

    nextState (SwapA w (coinA,coinB) swapAmount) = do
        let pool = liquidityPool (coinA,coinB)
        bc <- askModelState $ view $ balanceChange w
        mUniswap <- getUniswap

        mOldA <- getAmountForCoin pool coinA
        mOldB <- getAmountForCoin pool coinB
        mLiquidity <- getLiquidity pool
        case (mUniswap, liftM3 (,,) mOldA mOldB mLiquidity) of
            (Just us, Just (oldA, oldB, oldLiquidity)) -> do
                    when (tokenAmt + assetClassValueOf bc (unCoin coinA) >= swapAmount) $ do

                        let outAmount = findSwap (Amount oldA) (Amount oldB) (Amount swapAmount)
                        when (outAmount < oldB && outAmount > 0) $ do

                            amountForCoin pool coinA $= (oldA + swapAmount)
                            amountForCoin pool coinB $= (oldB - outAmount)

                            deposit w (valueOf coinB $ Amount outAmount)
                            withdraw w (valueOf coinA $ Amount swapAmount)
            _ -> return ()
        wait 1


    nextState (ISwapA w (coinA,coinB) swapAmount) = do
        bc <- askModelState $ view $ balanceChange w
        mUniswap <- getUniswap
        pools <- getPools
        case (mUniswap) of
            (Just us) -> do

                    when (tokenAmt + assetClassValueOf bc (unCoin coinA) >= swapAmount) $ do
                        let poolMap = (Map.fromList $ map (\((ca,a),(cb,b)) -> ((unCoin ca,unCoin cb),(a,b))) pools)
                        let mPath = findBestSwap poolMap (unCoin coinA, unCoin coinB) swapAmount

                        case mPath of
                            Nothing -> return ()
                            Just path -> do
                                s <- getModelState
                                let oldModelPools = s ^. contractState . modelPools


                                let step :: Maybe (Integer, Value.Value) -> (AssetClass,AssetClass) -> Spec UModel (Maybe (Integer,Value.Value))
                                    step Nothing _             = return Nothing
                                    step (Just (amount,value)) (ca,cb) = do

                                        let currentPool = (LiquidityPool (Coin ca) (Coin cb))
                                        mA <- getAmountForCoin currentPool (Coin ca)
                                        mB <- getAmountForCoin currentPool (Coin cb)
                                        case (mA, mB) of
                                            (Just a, Just b) -> do
                                                let outAmount = findSwap (Amount a) (Amount b) (Amount amount)
                                                if outAmount < b && outAmount > 0 && (tokenAmt + assetClassValueOf bc ca >= swapAmount)
                                                    then do
                                                        amountForCoin currentPool (Coin ca) $= (a+amount)
                                                        amountForCoin currentPool (Coin cb) $= (b-outAmount)
                                                        return $ Just (outAmount, value <> valueOf (Coin ca) (Amount amount) <> valueOf (Coin cb) (Amount (- outAmount)))
                                                    else return Nothing
                                            _ -> return Nothing
                                result <- foldM step (Just (swapAmount, mempty)) path
                                case result of
                                    Nothing -> modelPools $= oldModelPools
                                    Just (_,value) -> withdraw w value
            _ -> return ()
        wait 5

    nextState _ = return ()

    perform h _ cmd = case cmd of
        (StartA w)          -> callEndpoint @"start" (h $ StartKey w) uniswapCurrencySymbol >> delay 1
        (CreateA w (coinA,coinB) amountA amountB)   -> callEndpoint @"create"  (h $ UseKey w) (CreateParams coinA coinB (Amount amountA) (Amount amountB))    >> delay 1
        (AddA w (coinA,coinB) amountA amountB)  -> callEndpoint @"add" (h $ UseKey w) (AddParams coinA coinB (Amount amountA) (Amount amountB))               >> delay 1
        (RemoveA w (coinA,coinB) liquidity)  -> callEndpoint @"remove" (h $ UseKey w) (RemoveParams coinA coinB (Amount liquidity))                  >> delay 1
        (CloseA w (coinA,coinB)) -> callEndpoint @"close"   (h $ UseKey w) (CloseParams coinA coinB)   >> delay 1
        (SwapA w (coinA,coinB) amountA) -> callEndpoint @"swap" (h $ UseKey w) (SwapParams coinA coinB (Amount amountA)) >> delay 1
        (ISwapA w (coinA,coinB) amountA) -> callEndpoint @"iSwap" (h $ UseKey w) (IndirectSwapParams coinA coinB (Amount amountA))                   >> delay 5

    precondition s (StartA w)          = (isNothing $ s ^. contractState . uniswap)
    precondition s (CreateA _ (liquidityPool -> pool) a b)   = (a>0 && b>0) && (isJust $ s ^. contractState . uniswap) && (isNothing $ s ^. contractState . modelPools . at pool)
    precondition s (AddA _ (liquidityPool -> pool) a b) = (a>0 && b >0) && (isJust $ s ^. contractState . uniswap) && (isJust $ s ^. contractState . modelPools . at pool)
    precondition s (RemoveA _ (liquidityPool -> pool) l) = (isJust $ s ^. contractState . uniswap) && (isJust $ s ^. contractState . modelPools . at pool)
    precondition s (CloseA _ (liquidityPool -> pool)) = (isJust $ s ^. contractState . uniswap) && (isJust $ s ^. contractState . modelPools . at pool)
    precondition s (SwapA _ (liquidityPool -> pool) _) = (isJust $ s ^. contractState . uniswap) && (isJust $ s ^. contractState . modelPools . at pool)
    precondition s (ISwapA _ (coinA,coinB) _) = (isJust $ s ^. contractState . uniswap) && (Prelude.not $ null $ findPaths (unCoin coinA,unCoin coinB) $ map (\(LiquidityPool a b)->(unCoin a, unCoin b)) $ Map.keys (s ^. contractState . modelPools))
    precondition _ _ = False
deriving instance Eq (ContractInstanceKey UModel w s e)
deriving instance Show (ContractInstanceKey UModel w s e)


instanceSpec :: [ContractInstanceSpec UModel]
instanceSpec =
    [ContractInstanceSpec (StartKey (Wallet 1)) (Wallet 1) ownerEndpoint'] ++
    [ContractInstanceSpec (UseKey w) w $ (userEndpoints sampleUniswap) | w <- wallets]



delay :: Int -> EmulatorTrace ()
delay = void . waitNSlots . fromIntegral


wallets :: [Wallet]
wallets = map Wallet [2..10]

genWallet :: Gen Wallet
genWallet = elements wallets


genCoinPair :: Gen (Coin A, Coin B)
genCoinPair = do
    a <- elements coins
    b <- elements $ filter (/= a) coins
    return (a, (Coin $ unCoin b))



genEither :: Gen a -> Gen b -> Gen (Either a b)
genEither ga gb = do
    ga' <- ga
    gb' <- gb
    elements [Left ga', Right gb']

genNonNeg :: Gen Integer
genNonNeg = getNonNegative <$> arbitrary


genAmount :: Gen (Amount n)
genAmount = Amount <$> genNonNeg


customSymbolsAndTokens = [("ff","coin1"),("ee","coin2"),("dd","coin3"),("cc","coin4"),("bb","coin5")]
coins = map (Coin . uncurry Value.assetClass) customSymbolsAndTokens
[customCoin1,customCoin2,customCoin3,customCoin4,customCoin5] = coins

tokenAmt = 1_000_000_000

prop_U :: Actions UModel -> Property
prop_U = withMaxSuccess 100 . propRunActionsWithOptions
    (defaultCheckOptions & emulatorConfig .~ EmulatorConfig (Left d))
    instanceSpec
    (const $ pure True)
  where
    d :: InitialDistribution
    d = Map.fromList $ ( Wallet 1, lovelaceValueOf 1_000_000_000 <>
                         mconcat (map (\(symbol,tokenName) -> Value.singleton symbol tokenName tokenAmt) customSymbolsAndTokens) <>
                         nft
                       ) : [ ( w
                           , lovelaceValueOf 1_000_000_000 <>
                              mconcat (map (\(symbol,tokenName) -> Value.singleton symbol tokenName tokenAmt) customSymbolsAndTokens))
                           | w <- wallets
                           ]

test :: IO ()
test = quickCheck prop_U




---------- MOJE
nft = Value.singleton uniswapCurrencySymbol "Uniswap" 1
uniswapCurrencySymbol = "01"
uniswapNFTCoin = assetClass (currencySymbol "01") (tokenName "Uniswap")

sampleUniswap = Uniswap.OffChain.uniswap uniswapCurrencySymbol





