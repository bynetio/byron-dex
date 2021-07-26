{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main
  where

import Control.Exception    (Exception, throwIO)
import System.Environment   (lookupEnv, setEnv)
import Uniswap.App
import UniswapJsonApi.Types

data NoConfigFoundException = NoConfigFoundException deriving (Show, Exception)


mkAppContext :: IO AppContext
mkAppContext = do
  setEnv "APP_PORT" "3000"
  setEnv "APP_API_URL" "localhost"
  setEnv "APP_API_PORT" "8080" --FIXME remove this
  port    <- lookupEnv "APP_PORT"
  pabUrl  <- lookupEnv "APP_API_URL"
  pabPort <- lookupEnv "APP_API_PORT"
  let maybePab = MkPabConfig <$> pabUrl <*> (read <$> pabPort)
      maybeCtx = MkAppContext <$> maybePab <*> (read <$> port)
  case maybeCtx of
    Just ctx -> pure ctx
    Nothing  -> throwIO NoConfigFoundException

main :: IO ()
main = do
  context <- mkAppContext
  runApp context

