{-# LANGUAGE OverloadedStrings #-}

module UniswapJsonApi.Logic where

import           Data.Text
import           Servant

import           UniswapJsonApi.Model

create :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler ()
create coinA coinB amountA amountB = do
  throwError err500

swap :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler ()
swap coinA coinB amountA amountB = do
  throwError $ err500 { errBody = "not implemented yet" }

indirectSwap :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler ()
indirectSwap coinA coinB amountA amountB = do
  throwError $ err500 { errBody = "not implemented yet" }

close :: Maybe Text -> Maybe Text -> Handler ()
close coinA coinB = do
  throwError $ err500 { errBody = "not implemented yet" }

remove :: Maybe Text -> Maybe Text -> Maybe Int -> Handler ()
remove coinA coinB amount = do
  throwError $ err500 { errBody = "not implemented yet" }

add :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler ()
add coinA coinB amountA amountB = do
  throwError $ err500 { errBody = "not implemented yet" }

pools :: Handler ()
pools = do
  throwError $ err500 { errBody = "not implemented yet" }

funds :: Handler ()
funds = do
  throwError $ err500 { errBody = "not implemented yet" }

stop :: Handler ()
stop = do
  throwError $ err500 { errBody = "not implemented yet" }
