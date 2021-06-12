{-# LANGUAGE DeriveGeneric #-}

module UniswapJsonApi.Model where

import           Data.Aeson
import           Data.Text
import           GHC.Generics

-- wahtever model we'll need

newtype User = User { name :: Text } deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User
