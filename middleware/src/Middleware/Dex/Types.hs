{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module Middleware.Dex.Types where

import           Data.Aeson.Types (ToJSON)
import           Data.Text        (Text)
import           Dex.Types        (OrderInfo (..), fromNat)
import           GHC.Generics     (Generic)
import           Ledger           (AssetClass, CurrencySymbol, TokenName,
                                   TxOutRef)
import           Ledger.Value     (unAssetClass)

newtype Error = Error
  { errorMessage :: Text
  } deriving (Generic, ToJSON)

-- | Data type to represent wallet founds.
data FundView = FundView
  { coin   :: Coin
  , amount :: Integer
  } deriving (Show, Generic, ToJSON)

fundView :: AssetClass -> Integer -> FundView
fundView = FundView . coinFromAssetClass

data Coin = Coin
  { currencySymbol :: CurrencySymbol
  , tokenName      :: TokenName
  } deriving (Show, Generic, ToJSON)

coinFromAssetClass :: AssetClass -> Coin
coinFromAssetClass = uncurry Coin . unAssetClass

data DexOrder
  = DexOrder
      { orderHash    :: TxOutRef
      , lockedCoin   :: FundView
      , expectedCoin :: FundView
      , orderType    :: Text
      }
  deriving (Generic, Show, ToJSON)

dexOrder :: OrderInfo -> DexOrder
dexOrder OrderInfo { orderHash      = oh
                   , lockedCoin     = lc
                   , lockedAmount   = la
                   , expectedCoin   = ec
                   , expectedAmount = ea
                   , orderType      = ot } =
    DexOrder oh (fv lc la) (fv ec ea) ot
  where
    fv coin amount = fundView coin (fromNat amount)
