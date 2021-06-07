{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

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

withTheApp :: (Warp.Port -> IO ()) -> IO ()
withTheApp action = Warp.testWithApplication (pure usersApp) action

spec :: Spec
spec =
  around withTheApp $ do
    let createUser = client (Proxy :: Proxy UsersApi)
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

    describe "POST /users?name=<name>" $ do
      it "should create a user with a given name" $ \port -> do
        result <- runClientM (createUser $ Just "Test User") (clientEnv port)
        result `shouldBe` (Right $ User { name = "Test User" })

      it "should not create a user when name is not given" $ \port -> do
        result <- runClientM (createUser Nothing) (clientEnv port)
        result `shouldSatisfy` isLeft
