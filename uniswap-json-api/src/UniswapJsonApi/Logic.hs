{-# LANGUAGE OverloadedStrings #-}

module UniswapJsonApi.Logic where

import           Control.Monad.IO.Class
import           Data.Text
import           Servant

import           UniswapJsonApi.Model
import           UniswapJsonApi.Client

create :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler ()
create coinA coinB amountA amountB = do
  throwError err501

swap :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler ()
swap coinA coinB amountA amountB = do
  throwError err501

indirectSwap :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler ()
indirectSwap coinA coinB amountA amountB = do
  throwError err501

close :: Maybe Text -> Maybe Text -> Handler ()
close coinA coinB = do
  throwError err501

remove :: Maybe Text -> Maybe Text -> Maybe Int -> Handler ()
remove coinA coinB amount = do
  throwError err501

add :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler ()
add coinA coinB amountA amountB = do
  throwError err501

pools :: Handler ()
pools = do
  throwError err501

funds :: Handler ()
funds = do
  throwError err501

stop :: Handler ()
stop = do
  throwError err501

-- just for experiments purposes

todos :: Int -> Handler ()
todos i = do
  response <- liftIO $ fetchTodos i
  liftIO $ print response
  return ()

posts :: Int -> Handler ()
posts i = do
  response <- liftIO $ fetchPosts i
  liftIO $ print response
  return ()
