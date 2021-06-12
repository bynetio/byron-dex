module Main where

import           Control.Monad.Reader
import           UniswapJsonApi

config :: Config
config = Config { _port = 8080 }

main :: IO ()
main = runReader runApp config
