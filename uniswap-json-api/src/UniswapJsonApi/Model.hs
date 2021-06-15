{-# LANGUAGE OverloadedStrings #-}

module UniswapJsonApi.Model where

import           Data.Aeson
import qualified Data.Text as T

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
