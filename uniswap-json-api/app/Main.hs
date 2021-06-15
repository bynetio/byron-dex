{-# LANGUAGE OverloadedStrings #-}

module Main where

import           UniswapJsonApi.Model (Config(..))
import           UniswapJsonApi

config :: Config
config = Config { _port    = 8080
                , _apiUrl  = "jsonplaceholder.typicode.com"
                , _apiPort = 443
                }

main :: IO ()
main = runApp config
