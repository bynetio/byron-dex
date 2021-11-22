{-# LANGUAGE OverloadedStrings #-}

module CoinJSONCodecSpec (spec) where

import           Data.Aeson
import           Data.ByteString.Lazy.Internal
import           Middleware.Dex.Types
import           Test.Hspec

spec :: Spec
spec = do
  describe "toJSON" $ do
    it "returns Coin serialiased to JSON in symbol/name format" $ do
      byteCoin `shouldBe` encode someCoin
  describe "fromJSON" $ do
    it "returns a Coin deserialised from JSON in symbol/name format" $ do
      decode byteCoin `shouldBe` Just someCoin

byteCoin :: ByteString
byteCoin = "{\"name\":\"A\",\"symbol\":\"aa906c3a72afdd99d48a001f4c73cbf8cf54c62493e0d00774f32698\"}"

someCoin :: Coin
someCoin = Coin { currencySymbol = "aa906c3a72afdd99d48a001f4c73cbf8cf54c62493e0d00774f32698", tokenName = "A" }
