{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module UniswapJsonApiSpec (spec) where

import           Data.Either
import           Network.HTTP.Client      hiding (Proxy)
import qualified Network.Wai.Handler.Warp as Warp
import           Servant
import           Servant.Client
import           Servant.Server
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher

import           UniswapJsonApi
import           UniswapJsonApi.Logic
import           UniswapJsonApi.Model

config = Config 9999 "jsonplaceholder.typicode.com" 443

spec :: Spec
spec =
  with (pure $ swapApp config) $ do
    describe "GET /create..." $ do
      it "should end up with missing `coin_a` error" $
        get "/create?coin_a=A&coin_b=B&amount_a=1000&amount_b=1500" `shouldRespondWith` 405
      it "should end up with an error" $
        post "/create?coin_a=A&coin_b=B&amount_a=1000&amount_b=1500" "" `shouldRespondWith` 501

    describe "GET /swap..." $ do
      it "should end up with missing `coin_a` error" $
        get "/swap?coin_a=A&coin_b=B&amount_a=1000&amount_b=1500" `shouldRespondWith` 405
      it "should end up with an error" $
        post "/swap?coin_a=A&coin_b=B&amount_a=1000&amount_b=1500" "" `shouldRespondWith` 501

    describe "GET /indirect_swap..." $ do
      it "should end up with missing `coin_a` error" $
        get "/indirect_swap?coin_a=A&coin_b=B&amount_a=1000&amount_b=1500" `shouldRespondWith` 405
      it "should end up with an error" $
        post "/indirect_swap?coin_a=A&coin_b=B&amount_a=1000&amount_b=1500" "" `shouldRespondWith` 501

    describe "GET /close..." $ do
      it "should end up with missing `coin_a` error" $
        get "/close?coin_a=A&coin_b=B" `shouldRespondWith` 405
      it "should end up with an error" $
        post "/close?coin_a=A&coin_b=B" "" `shouldRespondWith` 501

    describe "GET /remove..." $ do
      it "should end up with missing `coin_a` error" $
        get "/remove?coin_a=A&coin_b=B&amount=1000" `shouldRespondWith` 405
      it "should end up with an error" $
        post "/remove?coin_a=A&coin_b=B&amount=1000" "" `shouldRespondWith` 501

    describe "GET /add..." $ do
      it "should end up with missing `coin_a` error" $
        get "/add?coin_a=A&coin_b=B&amount_a=1000&amount_b=1500" `shouldRespondWith` 405
      it "should end up with an error" $
        post "/add?coin_a=A&coin_b=B&amount_a=1000&amount_b=1500" "" `shouldRespondWith` 501

    describe "GET /pools" $ do
      it "should end up with an error" $
        get "/pools" `shouldRespondWith` 501

    describe "GET /funds" $ do
      it "should end up with an error" $
        get "/funds" `shouldRespondWith` 501

    describe "GET /stop" $ do
      it "should end up with an error" $
        get "/stop" `shouldRespondWith` 501

    describe "GET /todos/:id" $ do
      it "should end up with an error" $
        get "/todos/1" `shouldRespondWith` 200

    describe "GET /posts/:id" $ do
      it "should end up with an error" $
        get "/posts/1" `shouldRespondWith` 200

--       it "should be able to handle odd HTTP requests" $
--         request methodPost "/docs/501" [] "{" `shouldRespondWith` 405
--       it "we can also do more with the Response using hspec-wai's matchers" $
--         -- see also `MatchHeader` and JSON-matching tools as well...
--         get "/docs/1" `shouldRespondWith` 200 { matchBody = MatchBody bodyMatcher }
--
-- bodyMatcher :: [Network.HTTP.Types.Header] -> Body -> Maybe String
-- bodyMatcher _ body = case (decode body :: Maybe ()) of
--   -- success in this case means we return `Nothing`
--   Just val | val == Object $ HM.fromList [("a", String "b")] -> Nothing
--   _                                                          -> Just "This is how we represent failure: this message will be printed"
