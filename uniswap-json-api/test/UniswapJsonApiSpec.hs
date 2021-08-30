{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module UniswapJsonApiSpec (spec) where

import           Control.Monad.Freer          (Eff, runM)
import           Control.Monad.Freer.Error    (runError)
import           Data.Aeson
import           Data.Either
import qualified Data.HashMap.Strict          as HM
import           Network.HTTP.Client          hiding (Proxy)
import           Network.HTTP.Types
import qualified Network.Wai.Handler.Warp     as Warp
import           Servant
import           Servant.Client
import           Servant.Server
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher

import           Uniswap.API                  (API, api)
import           Uniswap.Common.AppError      (AppError, Err)
import           Uniswap.Common.Logger        (Logger, runColog)
import           Uniswap.Common.NextId        (NextId, runNextId)
import           Uniswap.Common.ServantClient (ServantClient,
                                               runServantClientUrl)
import           Uniswap.Common.Utils         (Time, runTime)
import           Uniswap.PAB                  (UniswapPab, runPab)
import           Uniswap.Types

type AppEffs =
  '[ UniswapPab
   , ServantClient
   , NextId
   , Logger
   , AppError
   , Time
   , Handler
   ]

swapApp :: Application
swapApp = serve (Proxy :: Proxy API) server
  where server        = hoistServer (Proxy :: Proxy API) liftToHandler api

liftToHandler :: forall a. Eff AppEffs a -> Handler a
liftToHandler effs = do
  handler <- runEffects effs
  either mapError pure handler

  where
    runEffects :: Eff AppEffs a -> Handler (Either Err a)
    runEffects = runM . runTime . runError . runColog . runNextId . runServantClientUrl toPabUrl . runPab

    toPabUrl :: String
    toPabUrl = "uniswap-json-api-test-url:3333"

mapError :: Err -> Handler a
mapError =
  throwError . \case
    err -> err400{errBody = encode . show $ err}-- FIXME: Map Errors to error codes/body

spec :: Spec
spec =
  with (pure swapApp) $ do
    describe "GET /create..." $ do
      it "should end up with missing `coin_a` error" $
        get "/create?coin_a=A&coin_b=B&amount_a=1000&amount_b=1500" `shouldRespondWith` 404
      it "should end up with an error" $
        post "/create?coin_a=A&coin_b=B&amount_a=1000&amount_b=1500" "" `shouldRespondWith` 404

    describe "GET /swap..." $ do
      it "should end up with missing `coin_a` error" $
        get "/swap?coin_a=A&coin_b=B&amount_a=1000&amount_b=1500" `shouldRespondWith` 404
      it "should end up with an error" $
        post "/swap?coin_a=A&coin_b=B&amount_a=1000&amount_b=1500" "" `shouldRespondWith` 404

    describe "GET /indirect_swap..." $ do
      it "should end up with missing `coin_a` error" $
        get "/indirect_swap?coin_a=A&coin_b=B&amount_a=1000&amount_b=1500" `shouldRespondWith` 404
      it "should end up with an error" $
        post "/indirect_swap?coin_a=A&coin_b=B&amount_a=1000&amount_b=1500" "" `shouldRespondWith` 404

    describe "GET /close..." $ do
      it "should end up with missing `coin_a` error" $
        get "/close?coin_a=A&coin_b=B" `shouldRespondWith` 404
      it "should end up with an error" $
        post "/close?coin_a=A&coin_b=B" "" `shouldRespondWith` 404

    describe "GET /remove..." $ do
      it "should end up with missing `coin_a` error" $
        get "/remove?coin_a=A&coin_b=B&amount=1000" `shouldRespondWith` 404
      it "should end up with an error" $
        post "/remove?coin_a=A&coin_b=B&amount=1000" "" `shouldRespondWith` 404

    describe "GET /add..." $ do
      it "should end up with missing `coin_a` error" $
        get "/add?coin_a=A&coin_b=B&amount_a=1000&amount_b=1500" `shouldRespondWith` 404
      it "should end up with an error" $
        post "/add?coin_a=A&coin_b=B&amount_a=1000&amount_b=1500" "" `shouldRespondWith` 404

    describe "GET /pools" $ do
      it "should end up with an error" $
        get "/pools" `shouldRespondWith` 404

    describe "GET /funds" $ do
      it "should end up with an error" $
        get "/funds" `shouldRespondWith` 404

    describe "GET /stop" $ do
      it "should end up with an error" $
        get "/stop" `shouldRespondWith` 404

    describe "GET /status" $ do
      it "should end up with an error" $
        get "/stop" `shouldRespondWith` 404

--       it "should be able to handle odd HTTP requests" $
--         request methodPost "/docs/501" [] "{" `shouldRespondWith` 405
--       it "we can also do more with the Response using hspec-wai's matchers" $
--         -- see also `MatchHeader` and JSON-matching tools as well...
--         get "/docs/1" `shouldRespondWith` 200 { matchBody = MatchBody bodyMatcher }
--
--bodyMatcher :: [Network.HTTP.Types.Header] -> Body -> Maybe String
--bodyMatcher _ body = case (decode body :: Maybe ()) of
--  -- success in this case means we return `Nothing`
--  Just val | val == Object $ HM.fromList [("a", String "b")] -> Nothing
--  _                                                          -> Just "This is how we represent failure: this message will be printed"
