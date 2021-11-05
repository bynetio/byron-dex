{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Middleware.Capability.Config where

import           Conferer
import           Conferer.FromConfig.Warp    ()
import qualified Conferer.Source.CLIArgs     as Cli
import qualified Conferer.Source.Dhall       as Dhall
import qualified Conferer.Source.Env         as Env
import           Control.Monad.Freer         (Eff, Members, interpret, send)
import           Control.Monad.Freer.TH      as Eff (makeEffect)
import           GHC.Generics
import           Middleware.Capability.Error
import qualified Network.Wai.Handler.Warp    as Warp

newtype AppConfig = AppConfig
  { appConfigServer :: Warp.Settings
  } deriving (Generic)

instance Conferer.FromConfig AppConfig

instance DefaultConfig AppConfig where
  configDef = AppConfig
    { appConfigServer = Warp.setPort 8080 configDef
    }

data ConfigLoader a where
  Load :: ConfigLoader AppConfig

Eff.makeEffect ''ConfigLoader

-- | Run "ConfigLoader" using IO
runConfigLoader :: Members [IO, Error AppError] r
  => Eff (ConfigLoader ': r) a
  -> Eff r a
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

sendOrThrow :: forall r a . Members [IO, Error AppError] r
            => IO a
            -> Eff r a
sendOrThrow io = send (try io) >>= either onFail pure
  where
    onFail :: IOException -> Eff r a
    onFail = throwError . ConfigLoaderError
