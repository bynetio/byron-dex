{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

-- |
module Middleware.Dex.Types where

import           Data.Aeson.Types   (FromJSON, ToJSON, parseJSON, toJSON,
                                     withObject, (.:))
import           Data.Ratio         (approxRational, denominator, numerator)
import           Data.Text          (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Dex.Types          (CancelOrderParams (CancelOrderParams),
                                     LiquidityOrderParams (LiquidityOrderParams),
                                     LiquidityPoolParams (LiquidityPoolParams),
                                     Nat (..), OrderInfo (..), PayoutSummary,
                                     PoolPartsParams (..), fromNat)
import           GHC.Generics       (Generic)
import           Ledger             (AssetClass, CurrencySymbol, TokenName,
                                     TxOutRef)
import qualified Ledger.Value       as LV (CurrencySymbol (..), TokenName (..),
                                           assetClass, currencySymbol,
                                           tokenName, unAssetClass,
                                           unCurrencySymbol)

newtype Error = Error
  { errorMessage :: Text
  }
  deriving (Generic, ToJSON)

data Coin = Coin
  { currencySymbol :: CurrencySymbol,
    tokenName      :: TokenName
  }
  deriving (Show, Generic)

instance FromJSON Coin where
  parseJSON = withObject "Coin" $ \v ->
    Coin
      <$> toSymbol (v .: "symbol")
      <*> toName (v .: "name")
    where
      toSymbol = fmap LV.currencySymbol
      toName = fmap $ LV.tokenName . encodeUtf8

instance ToJSON Coin where
  toJSON = toJSON . assetClassFromCoin

coinFromAssetClass :: AssetClass -> Coin
coinFromAssetClass = uncurry Coin . LV.unAssetClass

assetClassFromCoin :: Coin -> AssetClass
assetClassFromCoin (Coin cs tn) = LV.assetClass cs tn

-- FIXME implement ToJSON, FromJSON instances for Percentage
newtype Percentage = Percentage Double
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkPercentage :: (Nat, Nat) -> Percentage
mkPercentage (Nat x, Nat y) = Percentage $ calculatePercengate x y

pairFromDouble :: Double -> (Nat, Nat)
pairFromDouble d = (Nat $ numerator d', Nat $ denominator d')
  where
    d' = approxRational (d / 100) epsilon
    epsilon = 0.00001

calculatePercengate :: Integer -> Integer -> Double
calculatePercengate x y = fromInteger x / fromInteger y * 100

-- PARAMS

data CreateSellOrderParams = CreateSellOrderParams
  { lockedCoin     :: Coin,
    expectedCoin   :: Coin,
    lockedAmount   :: Integer,
    expectedAmount :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data CreateLiquidityPoolParams = CreateLiquidityPoolParams
  { coinA           :: Coin,
    coinB           :: Coin,
    amountA         :: Integer,
    poolPartsParams :: CreatePoolPartsParams,
    swapFee         :: Percentage,
    exchangeRate    :: Percentage
  }
  deriving (Generic, FromJSON, ToJSON, Show)

-- FIXME use JSON codecs instead of explicit type conversion
convertLiquidityPoolToPab :: CreateLiquidityPoolParams -> LiquidityPoolParams
convertLiquidityPoolToPab (CreateLiquidityPoolParams ca cb am ps (Percentage f) (Percentage e)) =
  LiquidityPoolParams (assetClassFromCoin ca) (assetClassFromCoin cb) (Nat am) (convertPoolToPab ps) (pairFromDouble f) (pairFromDouble e)

convertLiquidityPoolToMid :: LiquidityPoolParams -> CreateLiquidityPoolParams
convertLiquidityPoolToMid (LiquidityPoolParams ca cb (Nat a) ps f e) =
  CreateLiquidityPoolParams (coinFromAssetClass ca) (coinFromAssetClass cb) a (convertPoolToMid ps) (mkPercentage f) (mkPercentage e)

data CreateLiquidityOrderParams = CreateLiquidityOrderParams
  { lockedCoin     :: Coin,
    expectedCoin   :: Coin,
    lockedAmount   :: Integer,
    expectedAmount :: Integer,
    swapFee        :: Percentage
  }
  deriving (Generic, FromJSON, ToJSON, Show)

convertLiquidityOrderToPab :: CreateLiquidityOrderParams -> LiquidityOrderParams
convertLiquidityOrderToPab (CreateLiquidityOrderParams lc ec la ea (Percentage d)) =
  LiquidityOrderParams (assetClassFromCoin lc) (assetClassFromCoin ec) (Nat la) (Nat ea) (pairFromDouble d)

convertLiqudityOrderToMid :: LiquidityOrderParams -> CreateLiquidityOrderParams
convertLiqudityOrderToMid (LiquidityOrderParams lc ec (Nat la) (Nat ea) f) =
  CreateLiquidityOrderParams (coinFromAssetClass lc) (coinFromAssetClass ec) la ea (mkPercentage f)

data CreatePoolPartsParams = CreatePriceChangeParams
  { coinAPriceChange :: Percentage,
    coinBPriceChange :: Percentage,
    numberOfParts    :: Integer
  }
  deriving (FromJSON, Generic, Show, ToJSON)

convertPoolToPab :: CreatePoolPartsParams -> PoolPartsParams
convertPoolToPab (CreatePriceChangeParams (Percentage a) (Percentage b) np) =
  PriceChangeParams (pairFromDouble a) (pairFromDouble b) (Nat np)

convertPoolToMid :: PoolPartsParams -> CreatePoolPartsParams
convertPoolToMid (PriceChangeParams a b (Nat p)) =
  CreatePriceChangeParams (mkPercentage a) (mkPercentage b) p

newtype CancelOrderParams = CancelOrderParams TxOutRef
  deriving (FromJSON, Generic, Show, ToJSON)

newtype PerformRandomParams = PerformRandomParams Integer
  deriving (FromJSON, Generic, Show, ToJSON)

-- VIEWS

-- | Data type to represent wallet founds.
data FundView = FundView
  { coin   :: Coin,
    amount :: Integer
  }
  deriving (Show, Generic, ToJSON)

mkFundView :: AssetClass -> Integer -> FundView
mkFundView = FundView . coinFromAssetClass

data OrderView = OrderView
  { orderHash    :: TxOutRef,
    lockedCoin   :: FundView,
    expectedCoin :: FundView,
    orderType    :: Text
  }
  deriving (Generic, Show, ToJSON)

dexOrder :: OrderInfo -> OrderView
dexOrder
  OrderInfo
    { orderHash = oh,
      lockedCoin = lc,
      lockedAmount = la,
      expectedCoin = ec,
      expectedAmount = ea,
      orderType = ot
    } =
    OrderView oh (fv lc la) (fv ec ea) ot
    where
      fv coin amount = mkFundView coin (fromNat amount)

data PayoutView = PayoutView
  { coin   :: Coin,
    amount :: Integer
  }
  deriving (Show, Generic, ToJSON)

mkPayoutView :: AssetClass -> Integer -> PayoutView
mkPayoutView = PayoutView . coinFromAssetClass
