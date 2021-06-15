{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module UniswapJsonApi.Client where

import           Data.Aeson
import           Data.Proxy
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API
import           Servant.Client
import           System.Environment
import qualified Network.HTTP.Client as Network.HTTP.Client.Types

import           UniswapJsonApi.Model

baseApiUrl :: String
baseApiUrl = "jsonplaceholder.typicode.com"

baseApiPort :: Int
baseApiPort = 443

theManager :: IO Network.HTTP.Client.Types.Manager
theManager = newManager tlsManagerSettings

type PlaceholderAPI = "todos" :> Capture "id" Int :> Get '[JSON] PlaceholderTodoResponse
                 :<|> "posts" :> Capture "id" Int :> Get '[JSON] PlaceholderPostResponse

placeholderApi :: Proxy PlaceholderAPI
placeholderApi = Proxy

placeholder = client placeholderApi

todos :: Int -> ClientM PlaceholderTodoResponse
posts :: Int -> ClientM PlaceholderPostResponse
(todos :<|> posts) = placeholder

fetchTodos :: Int -> IO (Either ClientError PlaceholderTodoResponse)
fetchTodos i = do
  manager' <- theManager
  runClientM (todos i) (mkClientEnv manager' (BaseUrl Https baseApiUrl baseApiPort ""))

fetchPosts :: Int -> IO (Either ClientError PlaceholderPostResponse)
fetchPosts i = do
  manager' <- theManager
  runClientM (posts i) (mkClientEnv manager' (BaseUrl Https baseApiUrl baseApiPort ""))
