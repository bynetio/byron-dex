{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Spec.LiquidityPoolSpec where

import           Data.String
import           Ledger.Value
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Uniswap.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "LiquidityPoolSpec" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
  [
      testProperty "LiquidityPool a b fee s == LiquidityPool b a fee s" eqOrderOfCoinsNotMatterProp,
      testProperty "Order of coins in LiquidityPool does not matter during comparison" cmpOrderOfCoinsNotMatterProp
  ]

reCoin :: forall a b. Coin a -> Coin b
reCoin c = Coin $ unCoin c

eqOrderOfCoinsNotMatterProp :: Fee -> String -> Coin A -> Coin B -> Bool
eqOrderOfCoinsNotMatterProp fee feeStr coinA coinB =
    let lp1 = LiquidityPool coinA coinB fee (fromString feeStr)
        lp2 = LiquidityPool (reCoin coinB) (reCoin coinA) fee (fromString feeStr)
    in lp1 == lp2

cmpOrderOfCoinsNotMatterProp :: Fee -> String -> CoinPair -> CoinPair -> Bool
cmpOrderOfCoinsNotMatterProp fee feeStr cp1 cp2 =
    let (ca, cb)   = unPair cp1
        (ca', cb') = unPair cp2
        lp1 = LiquidityPool ca cb fee (fromString feeStr)
        lp2 = LiquidityPool ca' cb' fee (fromString feeStr)
        lp3 = LiquidityPool (reCoin cb) (reCoin ca) fee (fromString feeStr)
        lp4 = LiquidityPool (reCoin cb') (reCoin ca') fee (fromString feeStr)
        cmp = lp1 `compare` lp2
    in cmp == lp3 `compare` lp2  && cmp == lp1 `compare` lp4

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "liquidityPool constructs pools with a fixed coin order" $
      let a = mkCoin "ff" "a1"
          b = mkCoin "aa" "a1"
          fee = (12, 1000)
      in assertBool "smart constructor handles coin order" $ liquidityPool (a, b) fee == liquidityPool (reCoin b, reCoin a) fee
  ]

--------------Generators-------------------

hexChars :: [Char]
hexChars = ['a'..'f'] ++ ['0'..'9']

hexCharGen :: Gen Char
hexCharGen = elements hexChars

currencySymbolGen :: Gen CurrencySymbol
currencySymbolGen = fromString . concat <$> resize 7 (listOf pair)
  where
    pair :: Gen String
    pair = do
      n <- elements ['0'..'9']
      l <- elements ['a'..'f']
      return [l, n]

assetClassGen :: Gen AssetClass
assetClassGen = do
    cs <- currencySymbolGen
    assetClass cs . fromString <$> hexStr
  where
    hexStr :: Gen String
    hexStr = resize 2 (listOf hexCharGen)

coinGen :: Gen (Coin a)
coinGen = Coin <$> assetClassGen

newtype CoinPair = CoinPair { unPair :: (Coin A, Coin B) } deriving (Show, Eq)

coinPairGen :: Gen CoinPair
coinPairGen = do
    a <- coinGen
    b <- distinct a
    return $ CoinPair (a, b)
  where
    distinct :: forall a b. Coin a -> Gen (Coin b)
    distinct c = do
        c' <- coinGen
        if c' == c then distinct c else return $ Coin $ unCoin c'

instance Arbitrary (Coin a) where
    arbitrary :: Gen (Coin a)
    arbitrary = coinGen

instance Arbitrary CoinPair where
    arbitrary :: Gen CoinPair
    arbitrary = coinPairGen

instance Arbitrary LiquidityPool where
    arbitrary :: Gen LiquidityPool
    arbitrary = do
        CoinPair (a, b) <- coinPairGen
        fee <- arbitrary :: Gen (Integer, Integer)
        return $ liquidityPool (a, b) fee


