{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (lookupEnv)
import UniswapJsonApi (runApp)
import UniswapJsonApi.Model (Config (..))

fetchConfig :: IO (Maybe Config)
fetchConfig = do
  port <- lookupEnv "APP_PORT"
  apiUrl <- lookupEnv "APP_API_URL"
  apiPort <- lookupEnv "APP_API_PORT"
  return $ Config <$> (read <$> port) <*> apiUrl <*> (read <$> apiPort)

main :: IO ()
main = do
  config <- fetchConfig
  case config of
    Just c -> runApp c
    Nothing -> error "uniswap-json-api: no config available"
