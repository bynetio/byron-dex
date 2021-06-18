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

fetchTodos :: Config -> Int -> IO (Either ClientError PlaceholderTodoResponse)
fetchTodos c i = do
  let baseApiUrl  = _apiUrl c
  let baseApiPort = _apiPort c
  m <- theManager
  runClientM (todos i) (mkClientEnv m (BaseUrl Https baseApiUrl baseApiPort ""))

fetchPosts :: Config -> Int -> IO (Either ClientError PlaceholderPostResponse)
fetchPosts c i = do
  let baseApiUrl  = _apiUrl c
  let baseApiPort = _apiPort c
  m <- theManager
  runClientM (posts i) (mkClientEnv m (BaseUrl Https baseApiUrl baseApiPort ""))
