{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Middleware.Capability.Config where

import           Conferer
import           Conferer.FromConfig.Warp    ()
import qualified Conferer.Source.CLIArgs     as Cli
import qualified Conferer.Source.Dhall       as Dhall
import qualified Conferer.Source.Env         as Env
import           GHC.Generics
import           Middleware.Capability.Error
import qualified Network.Wai.Handler.Warp    as Warp
import           Polysemy
import qualified Polysemy.Internal           as P

newtype AppConfig = AppConfig
  { appConfigServer :: Warp.Settings
  } deriving (Generic)

instance Conferer.FromConfig AppConfig

instance DefaultConfig AppConfig where
  configDef = AppConfig
    { appConfigServer = Warp.setPort 8080 configDef
    }

data ConfigLoader m a where
  Load :: ConfigLoader m AppConfig

makeSem ''ConfigLoader

-- | Run "ConfigLoader" using IO
runConfigLoader :: Members [Embed IO, Error AppError] r
  => Sem (ConfigLoader ': r) a
  -> Sem r a
runConfigLoader = interpret $ \case
  Load -> do
    cfg <- sendOrThrow mkAppConfig
    sendOrThrow $ fetch cfg

mkAppConfig :: IO Config
mkAppConfig = mkConfig' []
  [ Cli.fromConfig
  , Env.fromConfig "middleware"
  , Dhall.fromFilePath "config.dhall"
  ]

sendOrThrow :: forall r a . Members [Embed IO, Error AppError] r
            => IO a
            -> Sem r a
sendOrThrow = fromExceptionVia ConfigLoaderError
