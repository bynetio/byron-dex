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

data UModel = UModel { _modelPools :: Map LiquidityPool (Integer,Integer,Integer)
                        , _uniswap :: Maybe Uniswap}
    deriving Show

makeLenses ''UModel

tests :: TestTree
tests = testProperty "uniswap model" prop_U

instance ContractModel UModel where

    data Action UModel =
              StartA Wallet
            | CreateA Wallet LiquidityPool Integer Integer
            | AddA Wallet LiquidityPool Integer Integer
            | RemoveA Wallet LiquidityPool Integer
            | CloseA Wallet LiquidityPool
            | SwapA Wallet LiquidityPool (Either Integer Integer)
            | ISwapA Wallet LiquidityPool (Either Integer Integer)
        deriving (Show, Eq)

    data ContractInstanceKey UModel w s e where
        StartKey :: Wallet           -> ContractInstanceKey UModel (Last (Either Text Uniswap)) UniswapOwnerSchema' Void
        UseKey   :: Wallet           -> ContractInstanceKey UModel (Last (Either Text UserContractState)) UniswapUserSchema    Void

    instanceTag key _ = fromString $ "instance tag for: " ++ show key

    arbitraryAction _ = oneof $
        (StartA <$> (pure $ Wallet 1)) :
        [ CreateA  <$> genWallet <*> genLiquidityPool <*> genNonNeg <*> genNonNeg ]               ++
        [ AddA <$> genWallet <*> genLiquidityPool <*> genNonNeg <*> genNonNeg ]               ++
        [ RemoveA <$> genWallet <*> genLiquidityPool <*> genNonNeg ]               ++
        [ CloseA  <$> genWallet <*> genLiquidityPool] ++
        [ SwapA <$> genWallet <*> genLiquidityPool <*> (genEither genNonNeg genNonNeg)] ++
        [ ISwapA <$> genWallet <*> genLiquidityPool <*> (genEither genNonNeg genNonNeg)]

    initialState = UModel Map.empty Nothing

    nextState (StartA w) = do
        withdraw w nft
        uniswap $= Just (Uniswap.OffChain.uniswap uniswapCurrencySymbol)
        wait 1




    nextState (CreateA w pool@LiquidityPool{..} amountA amountB) = do

        bc <- askModelState $ view $ balanceChange w
        mUniswap <- getUniswap
        mPool <- getPoolState pool
        case (mUniswap, mPool) of
            (Just us, Nothing)
                    | tokenAmt + assetClassValueOf bc (unCoin lpCoinA) >= amountA &&
                      tokenAmt + assetClassValueOf bc (unCoin lpCoinB) >= amountB
                -> do
                    let (fstCoin,sndCoin) = (Coin $ min (unCoin lpCoinA) (unCoin lpCoinB),Coin $ max (unCoin lpCoinA) (unCoin lpCoinB))
                    let (fstAmount,sndAmount) = if unCoin lpCoinA <= unCoin lpCoinB
                        then (amountA, amountB)
                        else (amountB, amountA)
                    let liquidity = calculateInitialLiquidity (Amount amountA) (Amount amountB)
                    let lC = liquidityCoin (fst $ unAssetClass $ unCoin $ usCoin us) fstCoin sndCoin
                    (modelPools . at pool) $= Just (fstAmount, sndAmount, unAmount liquidity)
                    withdraw w (valueOf lpCoinA (Amount amountA) <> valueOf lpCoinB (Amount amountB))
                    deposit w (Uniswap.Types.valueOf lC liquidity)
            _ -> return ()
        wait 1

    nextState (AddA w pool@LiquidityPool{..} amountA amountB) = do
        bc <- askModelState $ view $ balanceChange w
        mUniswap <- getUniswap
        mPool <- getPoolState pool
        case (mUniswap, mPool) of
            (Just us, Just (oldA, oldB, oldLiquidity))
                               | tokenAmt + assetClassValueOf bc (unCoin lpCoinA) >= amountA &&
                                 tokenAmt + assetClassValueOf bc (unCoin lpCoinB) >= amountB
                -> do
                    let (fstCoin,sndCoin) = (Coin $ min (unCoin lpCoinA) (unCoin lpCoinB),Coin $ max (unCoin lpCoinA) (unCoin lpCoinB))
                    let (fstAmount,sndAmount) = if unCoin lpCoinA <= unCoin lpCoinB
                        then (amountA, amountB)
                        else (amountB, amountA)

                    let delLiquidity = calculateAdditionalLiquidity (Amount oldA) (Amount oldB) (Amount oldLiquidity) (Amount fstAmount) (Amount sndAmount)
                    let lC = liquidityCoin (fst $ unAssetClass $ unCoin $ usCoin us) fstCoin sndCoin
                    (modelPools . ix pool) $= (fstAmount+oldA,sndAmount+oldB,unAmount delLiquidity + oldLiquidity)
                    withdraw w (valueOf lpCoinA (Amount amountA) <> valueOf lpCoinB (Amount amountB))
                    deposit w (Uniswap.Types.valueOf lC delLiquidity)
            _ -> return ()
        wait 1

    nextState (RemoveA w pool@LiquidityPool{..} rpDiff) = do
        bc <- askModelState $ view $ balanceChange w
        mUniswap <- getUniswap
        mPool <- getPoolState pool
        case (mUniswap, mPool) of
            (Just us, Just (oldA, oldB, oldLiquidity))
                               | rpDiff <= oldLiquidity
                -> do
                    let (fstCoin,sndCoin) = (Coin $ min (unCoin lpCoinA) (unCoin lpCoinB),Coin $ max (unCoin lpCoinA) (unCoin lpCoinB))
                    let lC = liquidityCoin (fst $ unAssetClass $ unCoin $ usCoin us) fstCoin sndCoin
                    when (rpDiff <= assetClassValueOf bc (unCoin lC) && rpDiff < oldLiquidity) $ do
                        let (Amount outA, Amount outB) = calculateRemoval (Amount oldA) (Amount oldB) (Amount oldLiquidity) (Amount rpDiff)
                        (modelPools . ix pool) $= (outA, outB ,oldLiquidity - rpDiff)
                        deposit w (valueOf fstCoin (Amount (oldA - outA)) <> valueOf sndCoin (Amount (oldB - outB)))
                        withdraw w (Uniswap.Types.valueOf lC (Amount rpDiff))
            _ -> return ()
        wait 1

    nextState (CloseA w pool@LiquidityPool{..}) = do
        bc <- askModelState $ view $ balanceChange w
        mUniswap <- getUniswap
        mPool <- getPoolState pool
        case (mUniswap, mPool) of
            (Just us, Just (oldA, oldB, oldLiquidity))
                -> do
                    let rpDiff = oldLiquidity
                    let (fstCoin,sndCoin) = (Coin $ min (unCoin lpCoinA) (unCoin lpCoinB),Coin $ max (unCoin lpCoinA) (unCoin lpCoinB))
                    let lC = liquidityCoin (fst $ unAssetClass $ unCoin $ usCoin us) fstCoin sndCoin
                    when (rpDiff == assetClassValueOf bc (unCoin lC)) $ do
                        (modelPools . at pool) $= Nothing
                        deposit w (valueOf fstCoin (Amount oldA) <> valueOf sndCoin (Amount oldB))
                        withdraw w (Uniswap.Types.valueOf lC (Amount rpDiff))
            _ -> return ()
        wait 1

    nextState (SwapA w pool@LiquidityPool{..} eAmount) = do
        bc <- askModelState $ view $ balanceChange w
        mUniswap <- getUniswap
        mPool <- getPoolState pool
        case (mUniswap, mPool, eAmount) of
            (Just us, Just (oldA, oldB, oldLiquidity), eAmount) -> do
                    let swapAmount = case eAmount of Left amount -> amount; Right amount -> amount

                    let (fstCoin,sndCoin) = (Coin $ min (unCoin lpCoinA) (unCoin lpCoinB),Coin $ max (unCoin lpCoinA) (unCoin lpCoinB))
                    let left = either (const $ unCoin lpCoinA) (const $ unCoin lpCoinB) eAmount == unCoin fstCoin
                    let (inCoin,outCoin) = if left then (fstCoin,sndCoin)else(sndCoin,fstCoin)

                    when (tokenAmt + assetClassValueOf bc (unCoin inCoin) >= swapAmount) $ do

                        let out = if left
                            then findSwapA (Amount oldA) (Amount oldB) (Amount swapAmount)
                            else findSwapB (Amount oldA) (Amount oldB) (Amount swapAmount)
                        let outAmount = if left then oldB else oldA
                        when (out < outAmount && out > 0) $ do
                            (modelPools . ix pool) $= if left
                                                        then (oldA + swapAmount, oldB - out, oldLiquidity)
                                                        else (oldA - out, oldB + swapAmount, oldLiquidity)

                            deposit w (valueOf outCoin $ Amount out)
                            withdraw w (valueOf inCoin $ Amount swapAmount)
            _ -> return ()
        wait 1


    nextState (ISwapA w pool@LiquidityPool{..} eAmount) = do
        bc <- askModelState $ view $ balanceChange w
        mUniswap <- getUniswap
        pools <- getPools
        let paths = findPaths (unCoin lpCoinA, unCoin lpCoinB) $ map (\((ca,_),(cb,_))->(unCoin ca,unCoin cb)) pools
        case (mUniswap, paths, eAmount) of
            (Just us, _:_ , eAmount) -> do
                    let swapAmount = case eAmount of Left amount -> amount; Right amount -> amount

                    let (fstCoin,sndCoin) = (Coin $ min (unCoin lpCoinA) (unCoin lpCoinB),Coin $ max (unCoin lpCoinA) (unCoin lpCoinB))


                    let left = either (const $ unCoin lpCoinA) (const $ unCoin lpCoinB) eAmount == unCoin fstCoin
                    let (inCoin,outCoin) = if left then (fstCoin,sndCoin)else(sndCoin,fstCoin)

                    when (tokenAmt + assetClassValueOf bc (unCoin inCoin) >= swapAmount) $ do
                        let poolMap = (Map.fromList $ map (\((ca,a),(cb,b)) -> ((unCoin ca,unCoin cb),(a,b))) pools)
                        let pairs = findBestSwap left poolMap swapAmount paths

                        case pairs of
                            [] -> return ()
                            ((x,_):_) -> do

                                let path :: [(AssetClass,AssetClass)]
                                    path = if left then x else reverse $ map (\(a,b)->(b,a)) x

                                s <- getModelState
                                let oldModelPools = s ^. contractState . modelPools
                                let findSwap a b c = if left
                                        then findSwapA (Amount a) (Amount b) (Amount c)
                                        else findSwapB (Amount a) (Amount b) (Amount c)
                                let step :: Maybe (Integer, Value.Value) -> (AssetClass,AssetClass) -> Spec UModel (Maybe (Integer,Value.Value))
                                    step Nothing _             = return Nothing
                                    step (Just (amount,value)) (ca,cb) = do
                                        let Just (a,b) = Map.lookup (ca,cb) poolMap
                                        let out = findSwap a b amount
                                        if out < b && out > 0 && (tokenAmt + assetClassValueOf bc ca >= swapAmount)
                                            then do
                                                let Just (_,_,liquidity) = s ^. contractState . modelPools . at (LiquidityPool (Coin ca) (Coin cb))
                                                (modelPools . ix (LiquidityPool (Coin ca) (Coin cb))) $= (a+amount, b-out, liquidity)
                                                return $ Just (out, value <> valueOf (Coin ca) (Amount amount) <> valueOf (Coin cb) (Amount (- out)))
                                            else return Nothing
                                result <- foldM step (Just (swapAmount, mempty)) path
                                case result of
                                    Nothing -> modelPools $= oldModelPools
                                    Just (_,value) -> withdraw w value
            _ -> return ()
        wait 1

    nextState _ = return ()

    perform h _ cmd = case cmd of
        (StartA w)          -> callEndpoint @"start" (h $ StartKey w) uniswapCurrencySymbol >> delay 1
        (CreateA w LiquidityPool{..} amountA amountB)   -> callEndpoint @"create"  (h $ UseKey w) (CreateParams lpCoinA lpCoinB (Amount amountA) (Amount amountB))    >> delay 1
        (AddA w LiquidityPool{..} amountA amountB)  -> callEndpoint @"add" (h $ UseKey w) (AddParams lpCoinA lpCoinB (Amount amountA) (Amount amountB))               >> delay 1
        (RemoveA w LiquidityPool{..} liquidity)  -> callEndpoint @"remove" (h $ UseKey w) (RemoveParams lpCoinA lpCoinB (Amount liquidity))                  >> delay 1
        (CloseA w LiquidityPool{..}) -> callEndpoint @"close"   (h $ UseKey w) (CloseParams lpCoinA lpCoinB)   >> delay 1
        (SwapA w LiquidityPool{..} (Left amountA)) -> callEndpoint @"swap" (h $ UseKey w) (SwapParams lpCoinA lpCoinB (Amount amountA) 0) >> delay 1
        (SwapA w LiquidityPool{..} (Right amountB)) -> callEndpoint @"swap" (h $ UseKey w) (SwapParams lpCoinA lpCoinB 0 (Amount amountB))                   >> delay 1
        (ISwapA w LiquidityPool{..} (Left amountA)) -> callEndpoint @"iSwap" (h $ UseKey w) (IndirectSwapParams lpCoinA lpCoinB (Amount amountA) 0)                   >> delay 5
        (ISwapA w LiquidityPool{..} (Right amountB)) -> callEndpoint @"iSwap" (h $ UseKey w) (IndirectSwapParams lpCoinA lpCoinB 0 (Amount amountB))                   >> delay 5

    precondition s (StartA w)          = (isNothing $ s ^. contractState . uniswap)
    precondition s (CreateA _ pool a b)   = (a>0 && b>0) && (isJust $ s ^. contractState . uniswap) && (isNothing $ s ^. contractState . modelPools . at pool)
    precondition s (AddA _ pool a b) = (a>0 && b >0) && (isJust $ s ^. contractState . uniswap) && (isJust $ s ^. contractState . modelPools . at pool)
    precondition s (RemoveA _ pool l) = (isJust $ s ^. contractState . uniswap) && (isJust $ s ^. contractState . modelPools . at pool)
    precondition s (CloseA _ pool) = (isJust $ s ^. contractState . uniswap) && (isJust $ s ^. contractState . modelPools . at pool)
    precondition s (SwapA _ pool _) = (isJust $ s ^. contractState . uniswap) && (isJust $ s ^. contractState . modelPools . at pool)
    --precondition s (ISwapA _ (LiquidityPool coinA coinB) _) = (isJust $ s ^. contractState . uniswap) && (Prelude.not $ null $ findPaths (unCoin coinA,unCoin coinB) $ map (\(LiquidityPool a b)->(unCoin a, unCoin b)) $ Map.keys (s ^. contractState . modelPools))
    precondition _ _ = False
deriving instance Eq (ContractInstanceKey UModel w s e)
deriving instance Show (ContractInstanceKey UModel w s e)


instanceSpec :: [ContractInstanceSpec UModel]
instanceSpec =
    [ContractInstanceSpec (StartKey (Wallet 1)) (Wallet 1) ownerEndpoint'] ++
    [ContractInstanceSpec (UseKey w) w $ (userEndpoints sampleUniswap) | w <- wallets]



delay :: Int -> EmulatorTrace ()
delay = void . waitNSlots . fromIntegral

getPoolState' :: ModelState UModel -> LiquidityPool -> Maybe (Integer,Integer,Integer)
getPoolState' s pool@LiquidityPool{..} = (s ^. contractState . modelPools . at pool)


sortPool LiquidityPool{..} = LiquidityPool (Coin newA) (Coin newB)
    where newA = min (unCoin lpCoinA) (unCoin lpCoinB)
          newB = max (unCoin lpCoinA) (unCoin lpCoinB)

getPoolState :: LiquidityPool -> Spec UModel (Maybe (Integer,Integer,Integer))
getPoolState pool = do
    s <- getModelState
    return $ getPoolState' s pool


getPools' :: ModelState UModel -> [((Coin A, Integer),(Coin B, Integer))]
getPools' s = map (\(LiquidityPool coinA coinB,(a,b,_)) -> ((coinA,a),(coinB,b))) (s ^@.. contractState . modelPools . itraversed)

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


wallets :: [Wallet]
wallets = map Wallet [2..10]

genWallet :: Gen Wallet
genWallet = elements wallets


genLiquidityPool :: Gen LiquidityPool
genLiquidityPool = do
    a <- elements coins
    b <- elements $ filter (/= a) coins
    return $ LiquidityPool a (Coin $ unCoin b)



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
