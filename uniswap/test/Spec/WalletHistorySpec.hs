module Spec.WalletHistorySpec (tests) where

import           Data.Text                    (Text, pack)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Uniswap.Common.WalletHistory

nonEmptyText :: Gen Text
nonEmptyText = pack <$> listOf1 arbitrary

instance Arbitrary a => Arbitrary (History a) where
  arbitrary = do
    a   <- arbitrary
    hid <- nonEmptyText
    oneof [ pure $ append hid a
          , pure $ remove hid
          ]

satisfyMonoidLaws :: TestTree
satisfyMonoidLaws =
  testGroup "monoid laws"
  [ testProperty "mempty <> x == x" leftIdentity
  , testProperty "x <> mempty == x" rightIdentity
  , testProperty "(x <> y) <> z == x <> (y <> z)" associativity
  ]

{-# ANN leftIdentity "HLint: ignore" #-}
leftIdentity :: History Integer -> Bool
leftIdentity x = mempty <> x == x

{-# ANN rightIdentity "HLint: ignore" #-}
rightIdentity :: History Integer -> Bool
rightIdentity x = x <> mempty == x

associativity :: History Integer -> History Integer -> History Integer -> Bool
associativity x y z = (x <> y) <> z == x <> (y <> z)

tests :: TestTree
tests = testGroup "Uniswap.Common.WalletHistory.History" [satisfyMonoidLaws]
