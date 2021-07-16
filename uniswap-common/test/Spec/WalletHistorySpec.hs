module Spec.WalletHistorySpec (spec) where

import Data.ByteString                      (ByteString, pack)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()
import Uniswap.Common.WalletHistory

nonEmptyBS :: Gen ByteString
nonEmptyBS = pack <$> listOf1 arbitrary

instance Arbitrary a => Arbitrary (History a) where
  arbitrary = do
    a   <- arbitrary
    hid <- nonEmptyBS
    oneof [ pure $ append hid a
          , pure $ remove hid
          ]

satisfyMonoidLaws :: (Eq a, Show a, Monoid a, Arbitrary a) => a -> Spec
satisfyMonoidLaws t = do
  describe "mempty" $ do
    it "is a left identity" $ property $ \x ->
      mempty <> x == x `asTypeOf` t

    it "is a right identity" $ property $ \x ->
      x <> mempty == x `asTypeOf` t

  describe "mappend" $ do
    it "is associative" $ property $ \x y z ->
      (x <> y) <> z == x <> (y <> z) `asTypeOf` t

spec :: Spec
spec = describe "Uniswap.Common.WalletHistory.History" $ do
    satisfyMonoidLaws (undefined :: History Integer)

