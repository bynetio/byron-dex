module Uniswap.IndirectSwaps where
import           Control.Monad    hiding (fmap)
import           Data.List        (foldl', sortOn)
import qualified Data.Map         as Map
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import           Prelude          (div, (^))
import           Uniswap.Pool
import           Uniswap.Types

findSwap :: Amount A -> Amount B -> Amount A -> Integer
findSwap oldA oldB inA
    | ub' <= 1   = 0
    | otherwise  = go 1 ub'
  where
    cs :: Integer -> Bool
    cs outB = checkSwap oldA oldB (oldA + inA) (oldB - Amount outB)

    ub' :: Integer
    ub' = head $ dropWhile cs [2 ^ i | i <- [0 :: Integer ..]]

    go :: Integer -> Integer -> Integer
    go lb ub
        | ub == (lb + 1) = lb
        | otherwise      =
      let
        m = div (ub + lb) 2
      in
        if cs m then go m ub else go lb m





findPaths :: (Eq c, Ord c) => (c,c) -> [(c,c)] -> [[(c,c)]]
findPaths (start,goal) pairs = go start (pairs ++ map (\(a,b)->(b,a)) pairs)
   where
         go a availablePairs
             | a == goal = [[]]
             | otherwise = do
            (a',b') <- availablePairs
            guard (a' == a)
            rest <- go b' (filter ((a' /=) . fst) availablePairs)
            return ((a',b') : rest)






findBestSwap pools (ca,cb) swapAmount =

                  case sortOn (negate . snd)
                    [(x,p) | (x,Just p) <- map (\x -> (x,price' swapAmount x)) paths]
                    of
                      []        -> Nothing
                      ((a,p):_) -> Just (a,p)

  where
        paths = findPaths (ca,cb) (Map.keys pools)
        allPools = Map.union pools (Map.fromList $ map (\((c1,c2),(a,b)) -> ((c2,c1),(b,a) )) $ Map.toList pools)
        price' initialSwapAmount path = foldl' (\maybeAmount (a,b) ->
                                          case maybeAmount of
                                            Nothing -> Nothing
                                            Just currentSwapAmount -> findSwap' currentSwapAmount (a,b)) (Just initialSwapAmount) path

        findSwap' currentSwapAmount pair = do
            (a,b) <- Map.lookup pair allPools
            let out = findSwap (Amount a) (Amount b) (Amount currentSwapAmount)
            guard (out > 0)
            return out


