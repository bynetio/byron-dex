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

module Uniswap.IndirectSwaps where
import           Control.Monad             hiding (fmap)
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
import           Prelude                   (Semigroup (..))
import           Text.Printf               (printf)
import           Uniswap.OnChain           (mkUniswapValidator,
                                            validateLiquidityForging)
import           Uniswap.Pool
import           Uniswap.Types

import           Control.Monad
import           Data.List                 (foldl', sortOn)
import qualified Data.Set                  as Set

findSwapA :: Amount A -> Amount B -> Amount A -> Integer
findSwapA oldA oldB inA
    | ub' <= 1   = 0
    | otherwise  = go 1 ub'
  where
    cs :: Integer -> Bool
    cs outB = checkSwap oldA oldB (oldA + inA) (oldB - Amount outB)

    ub' :: Integer
    ub' = head $ dropWhile cs [2 ^ i | i <- [0 :: Int ..]]

    go :: Integer -> Integer -> Integer
    go lb ub
        | ub == (lb + 1) = lb
        | otherwise      =
      let
        m = div (ub + lb) 2
      in
        if cs m then go m ub else go lb m

findSwapB :: Amount A -> Amount B -> Amount B -> Integer
findSwapB oldA oldB inB = findSwapA (switch oldB) (switch oldA) (switch inB)
  where
    switch = Amount . unAmount





findPaths :: (Eq c, Ord c) => (c,c) -> [(c,c)] -> [[(c,c)]]
findPaths (start,goal) pairs = go start pairs
   where
         go a availablePairs
             | a == goal = [[]]
             | otherwise = do
            (a',b') <- availablePairs
            guard (a' == a)
            rest <- go b' (filter ((a' /=) . fst) availablePairs)
            return ((a',b') : rest)





findBestSwap swapA pools swapAmount paths = sortOn ((negate <$>) . snd)
                    $ filter (\(_,x) -> x /= Nothing)
                    $ map (\x -> (x,price' swapAmount x)) paths

  where
        price' initialSwapAmount path = foldl' (\maybeAmount (a,b) ->
                                          case maybeAmount of
                                            Nothing -> Nothing
                                            Just currentSwapAmount -> findSwap currentSwapAmount (a,b)) (Just initialSwapAmount) path

        findSwap currentSwapAmount pair = do
            (a,b) <- Map.lookup pair pools
            if swapA
                then return $ findSwapA (Amount a) (Amount b) (Amount currentSwapAmount)
                else return $ findSwapA (Amount b) (Amount a) (Amount currentSwapAmount)
