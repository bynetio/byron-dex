{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Uniswap.IndirectSwaps where

import           Control.Applicative
import           Control.Monad          hiding (fmap)
import           Control.Monad.Freer
import           Control.Monad.Freer.TH (makeEffect)
import           Data.List              (foldl', sortOn)
import qualified Data.Map               as Map
import           Data.Maybe
import           Data.Text              (Text, pack)
import           PlutusTx.Prelude       hiding (Semigroup (..), unless)
import           Prelude                (div, (^))
import qualified Prelude
import           Uniswap.Pool
import           Uniswap.Types
findSwap :: Amount A -> Amount B -> Amount A -> Fee -> Integer

findSwap oldA oldB inA fee
  | ub' <= 1 = 0
  | otherwise = go 1 ub'
  where
    cs :: Integer -> Bool
    cs outB = checkSwap oldA oldB (oldA + inA) (oldB - Amount outB) fee

    ub' :: Integer
    ub' = head $ dropWhile cs [2 ^ i | i <- [0 :: Integer ..]]

    go :: Integer -> Integer -> Integer
    go lb ub
      | ub == (lb + 1) = lb
      | otherwise =
        let m = div (ub + lb) 2
         in if cs m then go m ub else go lb m

findPaths :: forall c. (Prelude.Eq c, Ord c) => (c, c) -> [(c, c, Fee)] -> [[(c, c, Fee)]]
findPaths (start, goal) keys = go start (keys ++ map (\(a, b, fee) -> (b, a, fee)) keys)
  where
    go :: c -> [(c,c,Fee)] -> [[(c,c,Fee)]]
    go a availablePairs
      | a == goal = [[]]
      | otherwise = do
        (a', b', fee) <- availablePairs
        guard (a' == a)
        rest <- go b' (filter (\pool -> pool Prelude./= (a', b', fee) && pool Prelude./= (b', a', fee)) availablePairs)
        return ((a', b', fee) : rest)

findBestSwap :: (Prelude.Ord b, Ord b) => Map.Map (b, b, Fee) (Integer, Integer) -> (b, b) -> Integer -> Maybe ([(b, b, Fee)], Integer)
findBestSwap pools (ca, cb) swapAmount =
  case
    map (\(a,(b,_)) -> (a,b))
    $ sortOn snd
    [(x, (- p, x)) | (x, Just p) <- map (\x -> (x, price' swapAmount x)) paths] of
    []           -> Nothing
    ((a, p) : _) -> Just (a, p)
  where
    paths = findPaths (ca, cb) (Map.keys pools)
    allPools = Map.union pools (Map.fromList $ map (\((c1, c2, fee), (a, b)) -> ((c2, c1, fee), (b, a))) $ Map.toList pools)
    price' initialSwapAmount path =
      foldl'
        ( \maybeAmount (a, b, fee) ->
            case maybeAmount of
              Nothing                -> Nothing
              Just currentSwapAmount -> findSwap' currentSwapAmount (a, b, fee)
        )
        (Just initialSwapAmount)
        path

    findSwap' currentSwapAmount (ca', cb', fee) = do
      (a, b) <- Map.lookup (ca', cb', fee) allPools
      let out = findSwap (Amount a) (Amount b) (Amount currentSwapAmount) fee
      guard (out > 0)
      return out


data IndirectSwapAction s r where
  IndirectSwapFail :: Text -> IndirectSwapAction s r
  IndirectSwapPools :: IndirectSwapAction s [LiquidityPoolWithCoins]
  IndirectSwapStep :: s -> (LiquidityPoolWithCoins, (Amount A, Amount B)) -> IndirectSwapAction s s
  IndirectSwapInitStepState :: IndirectSwapAction s s
  IndirectSwapCommit :: s -> IndirectSwapAction s ()
  IndirectSwapLog :: Text -> IndirectSwapAction s ()

indirectSwapFail :: forall s r. Text -> Eff '[IndirectSwapAction s] r
indirectSwapFail message = send $ IndirectSwapFail @s message

indirectSwapPools :: forall s r. Eff '[IndirectSwapAction s] [LiquidityPoolWithCoins]
indirectSwapPools = send $ IndirectSwapPools @s

indirectSwapStep :: forall s r. s -> (LiquidityPoolWithCoins, (Amount A, Amount B)) -> Eff '[IndirectSwapAction s] s
indirectSwapStep s p = send $ IndirectSwapStep s p

indirectSwapInitStepState :: forall s. Eff '[IndirectSwapAction s] s
indirectSwapInitStepState = send IndirectSwapInitStepState

indirectSwapCommit :: forall s. s -> Eff '[IndirectSwapAction s] ()
indirectSwapCommit s = send $ IndirectSwapCommit s

indirectSwapLog :: forall s. Text -> Eff '[IndirectSwapAction s] ()
indirectSwapLog m = send $ IndirectSwapLog @s m

iswap :: forall s. IndirectSwapParams -> Eff '[IndirectSwapAction s] ()
iswap IndirectSwapParams {..} = do
  when (amount <= 0) $
    indirectSwapFail @s "amount must be positive"

  pools <- indirectSwapPools @s
  let poolMap = Map.fromList $ map (\LiquidityPoolWithCoins{..} -> ((unCoin coinA, unCoin coinB, fee), (unAmount amountA, unAmount amountB))) pools
  (path, outB) <- case findBestSwap poolMap (unCoin coinA, unCoin coinB) (unAmount amount) of
    Nothing -> indirectSwapFail @s "path not found"
    Just p  -> return p
  when ((100 * outB `div` unAmount result) < (100 - slippage)) $
    indirectSwapFail @s "outB amount doesn't meet the slippage criteria"
  indirectSwapLog (pack $ Prelude.show path)
  initState <- indirectSwapInitStepState @s
  transaction <- foldM (indirectSwapStep @s) initState
    $ mapWithState (\swapAmount' LiquidityPoolWithCoins {..} -> let out = findSwap amountA amountB swapAmount' fee in ((LiquidityPoolWithCoins{..}, (swapAmount', Amount out)), Amount out)) amount
    $ map (\(a,b,f) -> let (amountA, amountB) = fromJust (Map.lookup (a,b,f) poolMap <|> ((\(b',a') -> (a',b')) Control.Applicative.<$> Map.lookup (b,a,f) poolMap)) in LiquidityPoolWithCoins (Coin a) (Coin b) f (Amount amountA) (Amount amountB)) path
  indirectSwapCommit @s transaction
  return ()



mapWithState :: (s -> a -> (b,s)) -> s -> [a] -> [b]
mapWithState f s [] = []
mapWithState f s (a:as) =
  let (b,s') = f s a
  in b : mapWithState f s' as
