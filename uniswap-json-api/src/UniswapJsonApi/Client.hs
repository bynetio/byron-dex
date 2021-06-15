{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module UniswapJsonApi.Client where

import           Data.Aeson
import           Data.Proxy
import qualified Data.Text               as T
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API
import           Servant.Client
import           System.Environment
import qualified Network.HTTP.Client as Network.HTTP.Client.Types

baseApiUrl :: String
baseApiUrl = "jsonplaceholder.typicode.com"

theManager :: IO Network.HTTP.Client.Types.Manager
theManager = newManager tlsManagerSettings

data PlaceholderTodoResponse =
  PlaceholderTodoResponse { tid        :: Int
                          , tuserId    :: Int
                          , ttitle     :: T.Text
                          , tcompleted :: Bool
                          } deriving (Eq, Show)

data PlaceholderPostResponse =
  PlaceholderPostResponse { pid     :: Int
                          , puserId :: Int
                          , ptitle  :: T.Text
                          , pbody   :: T.Text
                          } deriving (Eq, Show)

instance FromJSON PlaceholderTodoResponse where
  parseJSON = withObject "response" $ \o -> do
    i <- o .: "id"
    u <- o .: "userId"
    t <- o .: "title"
    c <- o .: "completed"
    return $ PlaceholderTodoResponse i u t c

instance FromJSON PlaceholderPostResponse where
  parseJSON = withObject "response" $ \o -> do
    i <- o .: "id"
    u <- o .: "userId"
    t <- o .: "title"
    b <- o .: "body"
    return $ PlaceholderPostResponse i u t b

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
  runClientM (todos i) (mkClientEnv manager' (BaseUrl Https baseApiUrl 443 ""))

fetchPosts :: Int -> IO (Either ClientError PlaceholderPostResponse)
fetchPosts i = do
  manager' <- theManager
  runClientM (posts i) (mkClientEnv manager' (BaseUrl Https baseApiUrl 443 ""))
