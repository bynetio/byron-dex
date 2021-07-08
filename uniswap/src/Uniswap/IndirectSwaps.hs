module Uniswap.IndirectSwaps where

import           Control.Monad    hiding (fmap)
import           Data.List        (foldl', sortOn)
import qualified Data.Map         as Map
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import           Prelude          (div, (^))
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

findPaths :: (Eq c, Ord c) => (c, c) -> [(c, c, Fee)] -> [[(c, c, Fee)]]
findPaths (start, goal) keys = go start (keys ++ map (\(a, b, fee) -> (b, a, fee)) keys)
  where
    go a availablePairs
      | a == goal = [[]]
      | otherwise = do
        (a', b', fee) <- availablePairs
        guard (a' == a)
        rest <- go b' (filter (\(a, _, _) -> a' /= a) availablePairs)
        return ((a', b', fee) : rest)

findBestSwap pools (ca, cb) swapAmount =
  case sortOn
    (negate . snd)
    [(x, p) | (x, Just p) <- map (\x -> (x, price' swapAmount x)) paths] of
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

    findSwap' currentSwapAmount (ca, cb, fee) = do
      (a, b) <- Map.lookup (ca, cb, fee) allPools
      let out = findSwap (Amount a) (Amount b) (Amount currentSwapAmount) fee
      guard (out > 0)
      return out
