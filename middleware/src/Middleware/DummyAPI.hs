{-# LANGUAGE DeriveGeneric #-}
-- | Dummy Servant API

module Middleware.DummyAPI where

import Data.Aeson
import GHC.Generics
import Polysemy
import Servant

data User = User
  { name :: String
  , age :: Int
  , email :: String
  } deriving (Eq, Show, Generic)

instance ToJSON User

type DummyAPI = "user" :> Get '[JSON] [User]

data Dummy r a where
  FetchUsers :: Dummy r [User]

makeSem ''Dummy

runDummy :: Sem (Dummy ': r) a -> Sem r a
runDummy = interpret (\case FetchUsers -> pure users)

users :: [User]
users =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk"
  , User "Albert Einstein" 136 "ae@mc2.org"
  ]

userAPI :: Proxy DummyAPI
userAPI = Proxy
