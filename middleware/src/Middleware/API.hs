{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |

module Middleware.API where

import Colog.Polysemy.Formatting   (WithLog)
import Data.Aeson
import GHC.Generics
import Ledger                      (AssetClass (..), CurrencySymbol, TokenName)
import Ledger.Value                (unAssetClass)
import Middleware.Capability.Error
import Middleware.PabClient
import Middleware.PabClient.Types
import Polysemy
import Servant

-- FIXME: Reorganize below types and function

-- | Data type to represent wallet founds. Move 'FundView' to separate module
data FundView = FundView
  { coin   :: Coin
  , amount :: Integer
  } deriving (Show, Generic, ToJSON)

data Coin = Coin
  { currencySymbol :: CurrencySymbol
  , tokenName      :: TokenName
  } deriving (Show, Generic, ToJSON)


type API = Capture "contract-instance-id" ContractInstanceId :> "funds" :> Get '[JSON] [FundView]

-- | Move ''
data Dex r a where
  Funds :: ContractInstanceId -> Dex r [FundView]

makeSem ''Dex

runDex :: (WithLog r, Members '[ManagePabClient] r)
       => Sem (Dex ': r) a
       -> Sem r a
runDex = interpret
  (\case
      Funds cid -> do
        fs <- getFunds cid
        pure $ fmap (\(assetClass, amount') ->
                       let coin' = uncurry Coin $ unAssetClass assetClass
                           fund' = FundView coin' amount'
                       in fund'
                       ) fs
  )


-- | Move 'server' to separate module
dexServer :: Members '[Error AppError, Dex] r => ServerT API (Sem r)
dexServer = funds
