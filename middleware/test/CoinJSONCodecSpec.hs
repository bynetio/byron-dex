{-# LANGUAGE OverloadedStrings #-}

module CoinJSONCodecSpec (spec) where

import           Data.Aeson
import           Data.ByteString.Lazy.Internal
import           Middleware.Dex.Types
import           Test.Hspec

spec :: Spec
spec = do
  describe "toJSON" $ do
    it "returns Coin serialiased to JSON in AssetClass format" $ do
      coinToPAB `shouldBe` encode someCoin
  describe "fromJSON" $ do
    it "returns a Coin deserialised from JSON in symbol/name format" $ do
      decode coinFromRequest `shouldBe` Just someCoin
  describe "fromJSON" $ do
    it "returns a Coin deserialised from JSON in AssetClass format" $ do
      decode coinToPAB `shouldBe` Just someCoin

coinFromRequest :: ByteString
coinFromRequest = "{ \"symbol\": \"aa906c3a72afdd99d48a001f4c73cbf8cf54c62493e0d00774f32698\", \"name\": \"A\" }"

coinToPAB :: ByteString
coinToPAB = "{\"unAssetClass\":[{\"unCurrencySymbol\":\"aa906c3a72afdd99d48a001f4c73cbf8cf54c62493e0d00774f32698\"},{\"unTokenName\":\"A\"}]}"

someCoin :: Coin
someCoin = Coin { currencySymbol = "aa906c3a72afdd99d48a001f4c73cbf8cf54c62493e0d00774f32698", tokenName = "A" }

instance Eq Coin
