module Spec.WalletHistorySpec (tests) where

import           Data.ByteString              (ByteString, pack)

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Uniswap.Common.WalletHistory

nonEmptyBS :: Gen ByteString
nonEmptyBS = pack <$> listOf1 arbitrary

instance Arbitrary a => Arbitrary (History a) where
  arbitrary = do
    a   <- arbitrary
    hid <- nonEmptyBS
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

leftIdentity :: History Integer -> Bool
leftIdentity x = mempty <> x == x

rightIdentity :: History Integer -> Bool
rightIdentity x = x <> mempty == x

associativity :: History Integer -> History Integer -> History Integer -> Bool
associativity x y z = (x <> y) <> z == x <> (y <> z)

tests :: TestTree
tests = testGroup "Uniswap.Common.WalletHistory.History" [satisfyMonoidLaws]
