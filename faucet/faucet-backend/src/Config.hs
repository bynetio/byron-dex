{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (parseCliConfig, CommandLineConfig(..), loadAppConfig, AppConfig(..), FaucetConfig(..), NodeConfig(..), SocketConfig(..), WalletConfig(..), MintConfig(..)) where

import qualified Conferer
import           Conferer.FromConfig.Warp ()
import qualified Conferer.Source.Dhall    as Dhall
import qualified Conferer.Source.Env      as Env
import           Data.Aeson               (KeyValue ((.=)), ToJSON (toJSON),
                                           object)
import           Data.Maybe               (fromMaybe)
import           GHC.Generics             (Generic)
import           Network.Wai.Handler.Warp (HostPreference)
import qualified Network.Wai.Handler.Warp as Warp
import           Options.Generic          (ParseRecord, Text, getRecord)

data WalletConfig = WalletConfig {
    walletConfigAddress  :: Text,
    walletConfigSkey     :: FilePath,
    walletConfigLovelace :: Integer
} deriving (Generic, ToJSON)

data MintConfig = MintConfig {
    mintConfigTokens   :: [Text],
    mintConfigQuantity :: Integer
} deriving (Generic, ToJSON)

data FaucetConfig = FaucetConfig {
    faucetConfigMint   :: MintConfig,
    faucetConfigWallet :: WalletConfig
} deriving (Generic, ToJSON)

newtype SocketConfig = SocketConfig {
    socketConfigPath :: FilePath
} deriving (Generic, ToJSON, Show)

data NodeConfig = NodeConfig {
    nodeConfigMagic   :: Maybe String,
    nodeConfigNetwork :: String,
    nodeConfigSocket  :: SocketConfig
} deriving (Generic, ToJSON,Show)

data AppConfig = AppConfig {
    appConfigServer :: Warp.Settings,
    appConfigFaucet :: FaucetConfig,
    appConfigNode   :: NodeConfig
} deriving (Generic)

instance ToJSON AppConfig where
    toJSON cfg = object
      [
        "server" .= object
          [
              "server" .= object
                [
                    "host" .= host cfg,
                    "port" .= port cfg
                ]
          ],
        "faucet" .= toJSON (appConfigFaucet cfg),
        "node" .= toJSON (appConfigNode cfg)
      ]
      where
          port = Warp.getPort . appConfigServer
          host :: AppConfig -> String
          host = parseHost . Warp.getHost . appConfigServer
            where
              parseHost :: HostPreference -> String
              parseHost p = case (words . show) p of
                _ : h : _ -> read h
                _         -> show p

instance Conferer.FromConfig NodeConfig
instance Conferer.FromConfig WalletConfig
instance Conferer.FromConfig FaucetConfig
instance Conferer.FromConfig SocketConfig
instance Conferer.FromConfig MintConfig
instance Conferer.FromConfig AppConfig

instance Conferer.DefaultConfig AppConfig where
  configDef = AppConfig {
      appConfigServer = Warp.setHost "0.0.0.0" $ Warp.setPort 8080 Conferer.configDef,
      appConfigFaucet = FaucetConfig (MintConfig ["coinA", "coinB"] 1000) (WalletConfig "" "/etc/faucet/payment.skey" 1500000),
      appConfigNode = NodeConfig (Just "1097911063") "Testnet" (SocketConfig "/var/node-ipc/socket")
    }

loadAppConfig :: CommandLineConfig -> IO AppConfig
loadAppConfig cliCfg = do
    cfg <- mkConfig
    Conferer.fetch cfg
  where
    mkConfig :: IO Conferer.Config
    mkConfig = Conferer.mkConfig' []
     [   Env.fromConfig "app"
       , Dhall.fromFilePath (configPathFrom cliCfg)
     ]


data CommandLineConfig = Start { config :: Maybe FilePath }
                        | ShowConfig { config :: Maybe FilePath }
                        deriving (Generic, Show)

instance ParseRecord CommandLineConfig

parseCliConfig :: IO CommandLineConfig
parseCliConfig = getRecord "Faucet App"

defaultConfigPath :: FilePath
defaultConfigPath = "/etc/faucet/app.config"

configPathFrom :: CommandLineConfig -> FilePath
configPathFrom (Start cfg)      = fromMaybe defaultConfigPath cfg
configPathFrom (ShowConfig cfg) = fromMaybe defaultConfigPath cfg
