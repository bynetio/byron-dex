{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Middleware.Config where

import           Conferer
import           Conferer.FromConfig.Warp ()
import           GHC.Generics

import qualified Conferer.Source.CLIArgs  as Cli
import qualified Conferer.Source.Dhall    as Dhall
import qualified Conferer.Source.Env      as Env

import qualified Network.Wai.Handler.Warp as Warp

mkAppConfig :: IO Config
mkAppConfig = mkConfig' []
  [ Cli.fromConfig
  , Env.fromConfig "middleware"
  , Dhall.fromFilePath "config.dhall"
  ]

newtype AppConfig = AppConfig
  { appConfigServer :: Warp.Settings
  } deriving (Generic)

instance Conferer.FromConfig AppConfig

instance DefaultConfig AppConfig where
  configDef = AppConfig
    { appConfigServer = Warp.setPort 8080 configDef
    }

-- todo: remove it after freer-simple integration
testMain :: IO ()
testMain = do
  config <- mkAppConfig
  appConfig <- fetch config
  let warpSettings = appConfigServer appConfig

  putStrLn $ "Running on port: " ++ show (Warp.getPort warpSettings)
  putStrLn $ "Bind to: " ++ show (Warp.getHost warpSettings)
