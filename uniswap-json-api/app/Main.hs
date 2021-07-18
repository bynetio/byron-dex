{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import Control.Exception    (Exception, throwIO)
import System.Environment   (lookupEnv, setEnv)
import UniswapJsonApi       (runApp)
import UniswapJsonApi.Types

data NoConfigFoundException = NoConfigFoundException deriving (Show, Exception)


mkAppContext :: IO AppContext
mkAppContext = do
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

