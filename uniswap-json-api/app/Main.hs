{-# LANGUAGE OverloadedStrings #-}

module Main where

import           UniswapJsonApi.Model (Config(..))
import           UniswapJsonApi

config :: Config
config = Config { _port    = 3000
                , _apiUrl  = "lambda"
                , _apiPort = 8080
                }

main :: IO ()
main = runApp config
