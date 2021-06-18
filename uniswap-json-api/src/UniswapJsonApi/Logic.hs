{-# LANGUAGE OverloadedStrings #-}

module UniswapJsonApi.Logic where

import           Control.Monad.IO.Class
import           Data.Text
import           Servant

import           UniswapJsonApi.Model
import           UniswapJsonApi.Client

create :: Config -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler ()
create _ coinA coinB amountA amountB = do
  throwError err501

swap :: Config -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler ()
swap _ coinA coinB amountA amountB = do
  throwError err501

indirectSwap :: Config -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler ()
indirectSwap _ coinA coinB amountA amountB = do
  throwError err501

close :: Config -> Maybe Text -> Maybe Text -> Handler ()
close _ coinA coinB = do
  throwError err501

remove :: Config -> Maybe Text -> Maybe Text -> Maybe Int -> Handler ()
remove _ coinA coinB amount = do
  throwError err501

add :: Config -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler ()
add _ coinA coinB amountA amountB = do
  throwError err501

pools :: Config -> Handler ()
pools _ = do
  throwError err501

funds :: Config -> Handler ()
funds _ = do
  throwError err501

stop :: Config -> Handler ()
stop _ = do
  throwError err501

-- just for experiments purposes

todos :: Config -> Int -> Handler ()
todos c i = do
  response <- liftIO $ fetchTodos c i
  liftIO $ print response
  return ()

posts :: Config -> Int -> Handler ()
posts c i = do
  response <- liftIO $ fetchPosts c i
  liftIO $ print response
  return ()
