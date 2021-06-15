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

data PlaceholderResponse =
  PlaceholderResponse { id        :: Int
                      , userId    :: Int
                      , title     :: T.Text
                      , completed :: Bool
                      } deriving (Eq, Show)

instance FromJSON PlaceholderResponse where
  parseJSON = withObject "response" $ \o -> do
    i <- o .: "id"
    u <- o .: "userId"
    t <- o .: "title"
    c <- o .: "completed"
    return $ PlaceholderResponse i u t c

type PlaceholderAPI = "todos"
  :> Capture "id" Int
  :> Get '[JSON] PlaceholderResponse

placeholderApi :: Proxy PlaceholderAPI
placeholderApi = Proxy

search = client placeholderApi

queries :: Int -> ClientM PlaceholderResponse
queries = search

main :: IO ()
main = do
  i <- getLine
  manager' <- newManager tlsManagerSettings
  res <- runClientM (queries (read i)) (mkClientEnv manager' (BaseUrl Https "jsonplaceholder.typicode.com" 443 ""))
  print res
