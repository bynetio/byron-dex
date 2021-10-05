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

import           Control.Foldl                      (generalize)
import           Control.Lens                       hiding (Swapped, elements)
import           Control.Monad
import           Control.Monad.Freer                hiding (interpose)
import           Control.Monad.Freer.Internal
import           Control.Monad.Freer.Writer
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (isJust, isNothing)
import           Data.String                        (IsString (..))
import           Data.Text                          (Text, unpack)
import           Data.Void
import           Debug.Trace
import           Ledger.Ada                         as Ada
import           Ledger.Value                       hiding (valueOf)
import           Plutus.Contract                    (awaitPromise)
import           Plutus.Contract.Test
import           Plutus.Contract.Test.ContractModel
import           Plutus.Trace.Emulator
import qualified Plutus.Trace.Emulator              as Trace
import qualified Plutus.V1.Ledger.Value             as Value
import           Prettyprinter
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import qualified Uniswap.Common.WalletHistory       as WH
import           Uniswap.IndirectSwaps
import           Uniswap.OffChain                   hiding (pools, uniswap)
import qualified Uniswap.OffChain
import           Uniswap.Pool
import           Uniswap.Types                      hiding (liquidityCoin)
import qualified Wallet.Emulator.Folds              as Folds

type PoolData = ((Coin A, Integer), (Coin B, Integer), Integer, Fee)

data UModel
  = UModel
      { _modelPools   :: Map LiquidityPool PoolData
      , _uniswap      :: Maybe Uniswap
      , _walletStates :: Map Wallet UserContractState
      , _poolsNumber  :: Int
      }
  deriving (Show)

makeLenses ''UModel

emptyUModel :: UModel
emptyUModel = UModel Map.empty Nothing Map.empty 0

tests :: TestTree
tests = testProperty "uniswap model" prop_U

test :: IO ()
test = quickCheck prop_U

amountForCoin :: Applicative f => LiquidityPool -> Coin a -> (Integer -> f Integer) -> UModel -> f UModel
amountForCoin pool@LiquidityPool {} coin = modelPools . at pool . _Just . getAmount
  where
    getAmount = lens get set''
    get ((ca, a), (_, b), _, _)
      | unCoin ca == unCoin coin = a
      | otherwise = b

    set'' ((ca, a), (cb, b), l, fee) v
      | unCoin ca == unCoin coin = ((ca, v), (cb, b), l, fee)
      | otherwise = ((ca, a), (cb, v), l, fee)

getAmountForCoin' :: ModelState UModel -> LiquidityPool -> Coin c -> Maybe Integer
getAmountForCoin' s pool coin = s ^? contractState . amountForCoin pool coin

getAmountForCoin :: LiquidityPool -> Coin c -> Spec UModel (Maybe Integer)
getAmountForCoin pool coin = do
  s <- getModelState
  return $ getAmountForCoin' s pool coin

getNumberOfPools' :: ModelState UModel -> Int
getNumberOfPools' s = s ^. contractState . poolsNumber

getNumberOfPools :: Spec UModel Int
getNumberOfPools = getNumberOfPools' <$> getModelState

mkPool :: (Coin A, Coin B) -> Integer -> Integer -> Integer -> Fee -> PoolData
mkPool (ca, cb) a b l fee = ((ca, fromIntegral a), (cb, fromIntegral b), l, fee)

liquidity :: Applicative f => LiquidityPool -> (Integer -> f Integer) -> UModel -> f UModel
liquidity pool = modelPools . ix pool . _3

getLiquidity' :: ModelState UModel -> LiquidityPool -> Maybe Integer
getLiquidity' s pool = s ^? contractState . liquidity pool

getLiquidity :: LiquidityPool -> Spec UModel (Maybe Integer)
getLiquidity pool = do
  s <- getModelState
  return $ getLiquidity' s pool

walletState ::
  Functor f =>
  Wallet ->
  (Maybe UserContractState -> f (Maybe UserContractState)) ->
  UModel ->
  f UModel
walletState w = walletStates . at w

getWalletState' :: ModelState UModel -> Wallet -> Maybe UserContractState
getWalletState' s w = s ^. contractState . walletState w

getWalletState :: Wallet -> Spec UModel (Maybe UserContractState)
getWalletState w = do
  s <- getModelState
  return $ getWalletState' s w

getPoolState' :: ModelState UModel -> LiquidityPool -> Maybe PoolData
getPoolState' s pool@LiquidityPool {} = s ^. contractState . modelPools . at pool

getPoolState :: LiquidityPool -> Spec UModel (Maybe PoolData)
getPoolState pool = do
  s <- getModelState
  return $ getPoolState' s pool

getPools' :: ModelState UModel -> [((Coin A, Integer), (Coin B, Integer), Fee)]
getPools' s = map (\(a, b, _, f) -> (a, b, f)) (s ^.. contractState . modelPools . traversed)

getPools :: Spec UModel [((Coin A, Integer), (Coin B, Integer), Fee)]
getPools = getPools' <$> getModelState

getUniswap' :: ModelState UModel -> Maybe Uniswap
getUniswap' s = s ^. contractState . uniswap

getUniswap :: Spec UModel (Maybe Uniswap)
getUniswap = getUniswap' <$> getModelState

instance ContractModel UModel where
  data Action UModel
    = StartA Wallet
    | CreateA Wallet (Coin A, Coin B, Fee) Integer Integer
    | AddA Wallet (Coin A, Coin B, Fee) Integer Integer
    | RemoveA Wallet (Coin A, Coin B, Fee) Integer
    | CloseA Wallet (Coin A, Coin B, Fee)
    | SwapA Wallet Integer
    | SwapPreviewA Wallet (Coin A, Coin B, Fee) Integer
    | ISwapA Wallet Integer
    | ISwapPreviewA Wallet (Coin A, Coin B) Integer
    deriving (Show, Eq)

  data ContractInstanceKey UModel w s e where
    StartKey :: Wallet -> ContractInstanceKey UModel (WH.History (Either Text Uniswap)) UniswapOwnerSchema' Void
    UseKey :: Wallet -> ContractInstanceKey UModel (WH.History (Either Text UserContractState)) UniswapUserSchema Void

  instanceTag key _ = fromString $ "instance tag for: " ++ show key

  arbitraryAction _ =
    oneof $
      pure (StartA $ knownWallet 1) :
      [CreateA <$> genWallet <*> genLPKey <*> genNonNeg <*> genNonNeg]
        ++ [AddA <$> genWallet <*> genLPKey <*> genNonNeg <*> genNonNeg]
        ++ [RemoveA <$> genWallet <*> genLPKey <*> genNonNeg]
        ++ [CloseA <$> genWallet <*> genLPKey]
        ++ [SwapA <$> genWallet <*> genNonNeg]
        ++ [SwapPreviewA <$> genWallet <*> genLPKey <*> genNonNeg]
        ++ [ISwapA <$> genWallet <*> genNonNeg]
        ++ [ISwapPreviewA <$> genWallet <*> genCoinPair <*> genNonNeg]

  initialState = emptyUModel

  nextState (StartA w) = do
    withdraw w nft
    uniswap $= Just (Uniswap.OffChain.uniswap uniswapCurrencySymbol)
    wait 2
  nextState (CreateA w (coinA, coinB, fee) amountA amountB) = do
    when
      ( amountA > 0
          && amountB > 0
          && fst fee > 0
          && snd fee > 0
          && unCoin coinA /= unCoin coinB
      )
      $ do
        let pool@LiquidityPool {..} = liquidityPool (coinA, coinB) fee
        bc <- askModelState $ view $ balanceChange w
        mUniswap <- getUniswap
        mPool <- getPoolState pool
        case (mUniswap, mPool) of
          (Just us, Nothing)
            | tokenAmt + assetClassValueOf bc (unCoin coinA) >= amountA
                && tokenAmt + assetClassValueOf bc (unCoin coinB) >= amountB ->
              do
                let liquidityAmount = calculateInitialLiquidity (Amount amountA) (Amount amountB)
                let lC = liquidityCoin (fst $ unAssetClass $ unCoin $ usCoin us) lpCoinA lpCoinB fee
                (modelPools . at pool) $= Just (mkPool (coinA, coinB) amountA amountB (unAmount liquidityAmount) fee)
                withdraw w (valueOf coinA (Amount amountA) <> valueOf coinB (Amount amountB))
                deposit w (Uniswap.Types.valueOf lC liquidityAmount)
                n <- getNumberOfPools
                poolsNumber $= (n + 1)
          _ -> return ()
    walletState w $= Just Created
    wait 2
  nextState (AddA w (coinA, coinB, fee) amountA amountB) = do
    when (amountA >= 0 && amountB >= 0) $ do
      let pool@LiquidityPool {..} = liquidityPool (coinA, coinB) fee
      bc <- askModelState $ view $ balanceChange w
      mUniswap <- getUniswap
      mOldA <- getAmountForCoin pool coinA
      mOldB <- getAmountForCoin pool coinB
      mLiquidity <- getLiquidity pool
      case (mUniswap, liftM3 (,,) mOldA mOldB mLiquidity) of
        (Just us, Just (oldA, oldB, oldLiquidity))
          | tokenAmt + assetClassValueOf bc (unCoin coinA) >= amountA
              && tokenAmt + assetClassValueOf bc (unCoin coinB) >= amountB ->
            do
              let delLiquidity = calculateAdditionalLiquidity (Amount oldA) (Amount oldB) (Amount oldLiquidity) (Amount amountA) (Amount amountB)
              when (delLiquidity > 0) $ do
                let lC = liquidityCoin (fst $ unAssetClass $ unCoin $ usCoin us) lpCoinA lpCoinB fee
                liquidity pool $= (oldLiquidity + fromIntegral delLiquidity)
                amountForCoin pool coinA $= (oldA + amountA)
                amountForCoin pool coinB $= (oldB + amountB)
                withdraw w (valueOf coinA (Amount amountA) <> valueOf coinB (Amount amountB))
                deposit w (Uniswap.Types.valueOf lC delLiquidity)
        _ -> return ()
    walletState w $= Just Added
    wait 2
  nextState (RemoveA w (coinA, coinB, fee) rpDiff) = do
    when (rpDiff > 0) $ do
      let pool@LiquidityPool {..} = liquidityPool (coinA, coinB) fee
      bc <- askModelState $ view $ balanceChange w
      mUniswap <- getUniswap
      mOldA <- getAmountForCoin pool coinA
      mOldB <- getAmountForCoin pool coinB
      mLiquidity <- getLiquidity pool
      case (mUniswap, liftM3 (,,) mOldA mOldB mLiquidity) of
        (Just us, Just (oldA, oldB, oldLiquidity))
          | rpDiff <= oldLiquidity ->
            do
              let lC = liquidityCoin (fst $ unAssetClass $ unCoin $ usCoin us) lpCoinA lpCoinB fee
              when (rpDiff <= assetClassValueOf bc (unCoin lC) && rpDiff < oldLiquidity) $ do
                let (Amount outA, Amount outB) = calculateRemoval (Amount oldA) (Amount oldB) (Amount oldLiquidity) (Amount rpDiff)

                liquidity pool $= (oldLiquidity - rpDiff)
                amountForCoin pool coinA $= outA
                amountForCoin pool coinB $= outB

                deposit w (valueOf coinA (Amount (oldA - outA)) <> valueOf coinB (Amount (oldB - outB)))
                withdraw w (Uniswap.Types.valueOf lC (Amount rpDiff))
        _ -> return ()
    walletState w $= Just Removed
    wait 2
  nextState (CloseA w (coinA, coinB, fee)) = do
    let pool = liquidityPool (coinA, coinB) fee
    bc <- askModelState $ view $ balanceChange w
    mUniswap <- getUniswap
    mOldA <- getAmountForCoin pool coinA
    mOldB <- getAmountForCoin pool coinB
    mLiquidity <- getLiquidity pool
    case (mUniswap, liftM3 (,,) mOldA mOldB mLiquidity) of
      (Just us, Just (oldA, oldB, oldLiquidity)) ->
        do
          let lC = liquidityCoin (fst $ unAssetClass $ unCoin $ usCoin us) coinA coinB fee
          when (oldLiquidity == assetClassValueOf bc (unCoin lC)) $ do
            (modelPools . at pool) $= Nothing
            deposit w (valueOf coinA (Amount oldA) <> valueOf coinB (Amount oldB))
            withdraw w (Uniswap.Types.valueOf lC (Amount oldLiquidity))
            n <- getNumberOfPools
            poolsNumber $= (n - 1)
      _ -> return ()
    walletState w $= Just Closed
    wait 2
  nextState (SwapPreviewA w (coinA, coinB, fee) swapAmount) = do
    result <- do
      let pool = liquidityPool (coinA, coinB) fee
      bc <- askModelState $ view $ balanceChange w
      mUniswap <- getUniswap

      mOldA <- getAmountForCoin pool coinA
      mOldB <- getAmountForCoin pool coinB
      mLiquidity <- getLiquidity pool
      case (mUniswap, liftM3 (,,) mOldA mOldB mLiquidity) of
        (Just _, Just (oldA, oldB, _)) | tokenAmt + assetClassValueOf bc (unCoin coinA) >= swapAmount -> do
          let outAmount = findSwap (Amount oldA) (Amount oldB) (Amount swapAmount) fee
          if outAmount > 0 && swapAmount > 0
            then return $ Just (SwapPreviewResult $ SwapPreviewResultData coinA (Amount swapAmount) coinB (Amount outAmount) fee)
            else return Nothing
        _ -> return Nothing
    walletState w $= result
    wait 2
  nextState (SwapA w slippage) = do
    state <- getWalletState w

    case state of
      Just (SwapPreviewResult (SwapPreviewResultData coinA (Amount swapAmount) coinB (Amount result) fee)) -> do
        let pool = liquidityPool (coinA, coinB) fee

        bc <- askModelState $ view $ balanceChange w
        mUniswap <- getUniswap

        mOldA <- getAmountForCoin pool coinA
        mOldB <- getAmountForCoin pool coinB
        mLiquidity <- getLiquidity pool
        case (mUniswap, liftM3 (,,) mOldA mOldB mLiquidity) of
          (Just _, Just (oldA, oldB, _)) | tokenAmt + assetClassValueOf bc (unCoin coinA) >= swapAmount -> do
            let outAmount = findSwap (Amount oldA) (Amount oldB) (Amount swapAmount) fee
            when (outAmount < oldB && outAmount > 0 && ((100 * outAmount `div` result) >= (100 - slippage))) $ do
              amountForCoin pool coinA $= (oldA + swapAmount)
              amountForCoin pool coinB $= (oldB - outAmount)

              deposit w (valueOf coinB $ Amount outAmount)
              withdraw w (valueOf coinA $ Amount swapAmount)
          _ -> return ()
      _ -> return ()
    walletState w $= Just Swapped
    wait 3
  nextState (ISwapPreviewA w (coinA, coinB) swapAmount) = do
    result <- do
      bc <- askModelState $ view $ balanceChange w
      mUniswap <- getUniswap
      pools <- getPools
      case mUniswap of
        (Just _) | tokenAmt + assetClassValueOf bc (unCoin coinA) >= swapAmount -> do
          let poolMap = Map.fromList $ map (\((ca, a), (cb, b), fee) -> ((unCoin ca, unCoin cb, fee), (a, b))) pools
          case findBestSwap poolMap (unCoin coinA, unCoin coinB) swapAmount of
            Just (_, outB) -> return $ Just (ISwapPreviewResult $ ISwapPreviewResultData coinA (Amount swapAmount) coinB (Amount outB))
            _ -> return Nothing
        _ -> return Nothing
    walletState w $= result
    wait 2
  nextState (ISwapA w slippage) = do
    state <- getWalletState w
    case state of
      Just (ISwapPreviewResult (ISwapPreviewResultData coinA swapAmount coinB result)) ->
        runM $
          replaceRelay
            pure
            ( \action k -> case action of
                IndirectSwapFail _ -> return ()
                IndirectSwapPools -> do
                  pools <- map (\((ca, a), (cb, b), f) -> LiquidityPoolWithCoins ca cb f (Amount a) (Amount b)) <$> sendM getPools
                  k pools
                IndirectSwapStep (liquidityPoolsWithCoins, _) (LiquidityPoolWithCoins {..}, (swapAmount', outAmount')) -> do
                  k (LiquidityPoolWithCoins coinA coinB fee (amountA + swapAmount') (amountB - outAmount') : liquidityPoolsWithCoins, outAmount')
                IndirectSwapInitStepState -> k ([], -10000)
                IndirectSwapCommit (liquidityPools, outAmount') -> do
                  sendM $ withdraw w (valueOf coinA swapAmount)
                  sendM $ deposit w (valueOf coinB outAmount')
                  sendM $
                    mapM_
                      ( \LiquidityPoolWithCoins {..} -> do
                          let pool = liquidityPool (coinA, coinB) fee
                          amountForCoin pool coinA $= unAmount amountA
                          amountForCoin pool coinB $= unAmount amountB
                          return ()
                      )
                      liquidityPools
                  k ()
                IndirectSwapLog message -> k ()
            )
            (iswap @([LiquidityPoolWithCoins], Amount B) $ IndirectSwapParams coinA coinB swapAmount result slippage)
      _ -> return ()
    walletState w $= Just ISwapped
    wait 6

  -- nextState (ISwapA w slippage) = do
  --     bc <- askModelState $ view $ balanceChange w
  --     mUniswap <- getUniswap
  --     pools <- getPools
  --     state <- getWalletState w
  --     case (mUniswap, state) of
  --         (Just _, Just (ISwapPreviewResult (ISwapPreviewResultData coinA (Amount swapAmount) coinB (Amount result))))
  --             | tokenAmt + assetClassValueOf bc (unCoin coinA) >= swapAmount -> do
  --                 let poolMap = Map.fromList $ map (\((ca,a),(cb,b),fee) -> ((unCoin ca,unCoin cb,fee),(a,b))) pools

  --                 case findBestSwap poolMap (unCoin coinA, unCoin coinB) swapAmount of
  --                     Nothing -> return ()
  --                     Just (path,outAmount) | (100*outAmount `div` result) >= (100 - slippage) -> do
  --                         s <- getModelState
  --                         let oldModelPools = s ^. contractState . modelPools

  --                         let step :: Maybe Integer -> (AssetClass,AssetClass,Fee) -> Spec UModel (Maybe Integer)
  --                             step Nothing _             = return Nothing
  --                             step (Just amount) (ca,cb,fee) = do

  --                                 let currentPool = liquidityPool (Coin ca, Coin cb) fee
  --                                 mA <- getAmountForCoin currentPool (Coin ca)
  --                                 mB <- getAmountForCoin currentPool (Coin cb)
  --                                 case (mA, mB) of
  --                                     (Just a, Just b) -> do
  --                                         let outAmount' = findSwap (Amount a) (Amount b) (Amount amount) fee
  --                                         if outAmount' < b && outAmount' > 0 && (tokenAmt + assetClassValueOf bc ca >= amount)
  --                                             then do
  --                                                 amountForCoin currentPool (Coin ca) $= (a+amount)
  --                                                 amountForCoin currentPool (Coin cb) $= (b-outAmount')
  --                                                 return $ Just outAmount'
  --                                             else return Nothing
  --                                     _ -> return Nothing
  --                         result' <- foldM step (Just swapAmount) path
  --                         case result' of
  --                             Nothing        -> modelPools $= oldModelPools
  --                             Just outAmount' -> withdraw w (valueOf coinA (Amount swapAmount) <> valueOf coinB (Amount (- outAmount')))
  --                     _ -> return ()
  --         _ -> return ()
  --     walletState w $= Just ISwapped
  --     wait 6

  perform h _ cmd = case cmd of
    (StartA w) -> callEndpoint @"start" (h $ StartKey w) (WithHistoryId "" uniswapCurrencySymbol) >> delay 2
    (CreateA w (coinA, coinB, fee) amountA amountB) -> callEndpoint @"create" (h $ UseKey w) (WithHistoryId "" $ CreateParams coinA coinB fee (Amount amountA) (Amount amountB)) >> delay 2
    (AddA w (coinA, coinB, fee) amountA amountB) -> callEndpoint @"add" (h $ UseKey w) (WithHistoryId "" $ AddParams coinA coinB fee (Amount amountA) (Amount amountB)) >> delay 2
    (RemoveA w (coinA, coinB, fee) liquidity') -> callEndpoint @"remove" (h $ UseKey w) (WithHistoryId "" $ RemoveParams coinA coinB fee (Amount liquidity')) >> delay 2
    (CloseA w (coinA, coinB, fee)) -> callEndpoint @"close" (h $ UseKey w) (WithHistoryId "" $ CloseParams coinA coinB fee) >> delay 2
    (SwapA w slippage) -> swapTrace w slippage
    (SwapPreviewA w (coinA, coinB, fee) amountA) -> callEndpoint @"swapPreview" (h $ UseKey w) (WithHistoryId "swapPreview" $ SwapPreviewParams coinA coinB fee (Amount amountA)) >> delay 2
    (ISwapA w slippage) -> iSwapTrace w slippage
    (ISwapPreviewA w (coinA, coinB) amountA) -> callEndpoint @"iSwapPreview" (h $ UseKey w) (WithHistoryId "iSwapPreview" $ ISwapPreviewParams coinA coinB (Amount amountA)) >> delay 2
    where
      swapTrace w slippage = do
        state <- WH.lookup "swapPreview" <$> observableState (h $ UseKey w)
        case state of
          Just (Right (SwapPreviewResult SwapPreviewResultData {..})) -> callEndpoint @"swap" (h $ UseKey w) (WithHistoryId "" $ SwapParams coinA coinB fee amountA amountB slippage)
          _ -> return ()
        delay 2
        callEndpoint @"clearState" (h $ UseKey w) (WithHistoryId "" $ ClearStateParams "swapPreview")
        delay 1

      iSwapTrace w slippage = do
        state <- WH.lookup "iSwapPreview" <$> observableState (h $ UseKey w)
        case state of
          Just (Right (ISwapPreviewResult ISwapPreviewResultData {..})) -> callEndpoint @"iSwap" (h $ UseKey w) (WithHistoryId "" $ IndirectSwapParams coinA coinB amountA amountB slippage)
          _ -> return ()
        delay 5
        callEndpoint @"clearState" (h $ UseKey w) (WithHistoryId "" $ ClearStateParams "iSwapPreview")
        delay 1

  precondition s (StartA _) = isNothing $ s ^. contractState . uniswap
  --precondition s (CreateA _ (uncurry liquidityPool . (\(a,b,f) -> ((a,b),f)) -> pool) a b)   = (a>0 && b>0) && (isJust $ s ^. contractState . uniswap) && (isNothing $ s ^. contractState . modelPools . at pool)
  --precondition s (AddA _ (uncurry  liquidityPool . (\(a,b,f) -> ((a,b),f)) -> pool) a b) = (a>0 && b >0) && (isJust $ s ^. contractState . uniswap) && (isJust $ s ^. contractState . modelPools . at pool)
  --precondition s (RemoveA _ (uncurry liquidityPool . (\(a,b,f) -> ((a,b),f)) -> pool) l) = (isJust $ s ^. contractState . uniswap) && (isJust $ s ^. contractState . modelPools . at pool)
  --precondition s (CloseA _ (uncurry liquidityPool . (\(a,b,f) -> ((a,b),f)) -> pool)) = (isJust $ s ^. contractState . uniswap) && (isJust $ s ^. contractState . modelPools . at pool)
  precondition s (SwapA w _) =
    isJust (s ^. contractState . uniswap)
      && (case s ^. contractState . walletState w of Just (SwapPreviewResult _) -> True; _ -> False)
  --precondition s (SwapPreviewA _ (uncurry liquidityPool . (\(a,b,f) -> ((a,b),f)) -> pool) _) = (isJust $ s ^. contractState . uniswap) && (isJust $ s ^. contractState . modelPools . at pool)
  precondition s (ISwapA w _) =
    isJust (s ^. contractState . uniswap)
      && (case s ^. contractState . walletState w of Just (ISwapPreviewResult _) -> True; _ -> False)
  --precondition s (ISwapPreviewA _ (coinA,coinB) _) = (isJust $ s ^. contractState . uniswap) && (Prelude.not $ null $ findPaths (unCoin coinA,unCoin coinB) $ map (\(LiquidityPool a b fee _)->(unCoin a, unCoin b, fee)) $ Map.keys (s ^. contractState . modelPools))
  precondition s _ = isJust $ s ^. contractState . uniswap

deriving instance Eq (ContractInstanceKey UModel w s e)

deriving instance Show (ContractInstanceKey UModel w s e)

instanceSpec :: [ContractInstanceSpec UModel]
instanceSpec =
  ContractInstanceSpec (StartKey (knownWallet 1)) (knownWallet 1) (awaitPromise ownerEndpoint') :
    [ContractInstanceSpec (UseKey w) w $ awaitPromise $ userEndpoints sampleUniswap | w <- wallets]

delay :: Int -> EmulatorTrace ()
delay = void . waitNSlots . fromIntegral

wallets :: [Wallet]
wallets = map knownWallet [2 .. 3]

genWallet :: Gen Wallet
genWallet = elements wallets

fees :: [(Integer, Integer)]
fees = [(3, 1000), (1, 1000)]

genCoinPair :: Gen (Coin A, Coin B)
genCoinPair = do
  a <- elements coins
  b <- elements $ filter (/= a) coins
  return (a, Coin $ unCoin b)

genLPKey :: Gen (Coin A, Coin B, (Integer, Integer))
genLPKey = do
  (a, b) <- genCoinPair
  fee <- elements fees
  return (a, b, fee)

genEither :: Gen a -> Gen b -> Gen (Either a b)
genEither ga gb = do
  ga' <- ga
  gb' <- gb
  elements [Left ga', Right gb']

genNonNeg :: Gen Integer
genNonNeg = abs <$> arbitrary

genAmount :: Gen (Amount n)
genAmount = Amount <$> genNonNeg

customSymbolsAndTokens :: [(CurrencySymbol, TokenName)]
customSymbolsAndTokens = [("ff", "coin1"), ("ee", "coin2"), ("dd", "coin3")] --,("cc","coin4"),("bb","coin5")]

coins :: [Coin A]
coins = map (Coin . uncurry Value.assetClass) customSymbolsAndTokens

customCoin1, customCoin2, customCoin3, customCoin4, customCoin5 :: Coin A
[customCoin1, customCoin2, customCoin3, customCoin4, customCoin5] = coins

tokenAmt :: Integer
tokenAmt = 1_000_000_000

prop_U :: Actions UModel -> Property
prop_U =
  withMaxSuccess 1000
    . propRunActionsWithOptions
      (defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Left d)
      instanceSpec
      (\s -> flip Folds.postMapM (generalize Folds.userLog) $ \lg -> tell @(Doc Void) (pretty $ show (s ^. contractState)) >> return True)
  where
    d :: InitialDistribution
    d =
      Map.fromList $
        ( knownWallet 1,
          lovelaceValueOf 1_000_000_000
            <> mconcat (map (\(symbol, tn) -> Value.singleton symbol tn tokenAmt) customSymbolsAndTokens)
            <> nft
        ) :
          [ ( w,
              lovelaceValueOf 1_000_000_000
                <> mconcat (map (\(symbol, tn) -> Value.singleton symbol tn tokenAmt) customSymbolsAndTokens)
            )
            | w <- wallets
          ]

nft :: Value
nft = Value.singleton uniswapCurrencySymbol "Uniswap" 1

uniswapCurrencySymbol :: CurrencySymbol
uniswapCurrencySymbol = "01"

uniswapNFTCoin :: AssetClass
uniswapNFTCoin = assetClass (currencySymbol "01") (tokenName "Uniswap")

sampleUniswap :: Uniswap
sampleUniswap = Uniswap.OffChain.uniswap uniswapCurrencySymbol
