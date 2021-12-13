{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
module Middleware.Dex.Types where

import           Data.Aeson.Types           (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             (.:), (.=))
import           Data.OpenApi.Schema        (ToSchema)
import           Data.Ratio                 (approxRational, denominator,
                                             numerator)
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Dex.Types                  (AssetSet (AssetSet),
                                             CancelOrderParams (CancelOrderParams),
                                             LiquidityOrderParams (LiquidityOrderParams),
                                             LiquidityPoolParams (LiquidityPoolParams),
                                             Nat (..), OrderInfo (..),
                                             PayoutSummary,
                                             PoolPartsParams (..),
                                             SellOrderParams (..), fromNat)
import           GHC.Generics               (Generic)
import           Ledger                     (AssetClass, CurrencySymbol,
                                             TokenName, TxOutRef)
import qualified Ledger.Value               as LV (assetClass, currencySymbol,
                                                   tokenName, unAssetClass,
                                                   unCurrencySymbol,
                                                   unTokenName)
import           PlutusTx.Builtins.Internal (BuiltinByteString (..))

newtype Error = Error
  { errorMessage :: Text
  }
  deriving (Generic)
  deriving anyclass (ToJSON)


newtype ActivateForm = ActivateForm { walletId :: WalletId }
    deriving (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype WalletId = WalletId Text
    deriving stock (Show, Generic)
    deriving newtype (FromJSON, ToJSON, ToSchema)

data Coin = Coin
  { currencySymbol :: CurrencySymbol,
    tokenName      :: TokenName
  }
  deriving (Show, Eq, Generic, ToSchema)

instance FromJSON Coin where
  parseJSON = withObject "Coin" $ \v ->
    Coin
      <$> toSymbol (v .: "currencySymbol")
      <*> toName (v .: "tokenName")
    where
      toSymbol = fmap LV.currencySymbol
      toName = fmap $ LV.tokenName . encodeUtf8

instance ToJSON Coin where
  toJSON coin =
    object
      [ "currencySymbol" .= toJSON (LV.unCurrencySymbol . currencySymbol $ coin),
        "tokenName" .= toJSON (decodeUtf8 . unBSS . LV.unTokenName . tokenName $ coin)
      ]
      where
        unBSS (BuiltinByteString s) = s

coinFromAssetClass :: AssetClass -> Coin
coinFromAssetClass = uncurry Coin . LV.unAssetClass

assetClassFromCoin :: Coin -> AssetClass
assetClassFromCoin (Coin cs tn) = LV.assetClass cs tn

data CoinSet
  = CoinSet
  { lockedCoin   :: Coin,
    expectedCoin :: Coin
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

convertCoinSetToPab :: CoinSet -> AssetSet
convertCoinSetToPab (CoinSet lc ec) = AssetSet (assetClassFromCoin lc) (assetClassFromCoin ec)

mkCoinSet :: AssetSet -> CoinSet
mkCoinSet (AssetSet lc ec) = CoinSet (coinFromAssetClass lc) (coinFromAssetClass ec)

newtype Percentage = Percentage Double
    deriving (Show, Generic, Eq)
    deriving newtype (ToSchema)
    deriving anyclass (FromJSON, ToJSON)

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
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

convertSellOrderToPab :: CreateSellOrderParams -> SellOrderParams
convertSellOrderToPab (CreateSellOrderParams lc ec la ea) =
  SellOrderParams (assetClassFromCoin lc) (assetClassFromCoin ec) (Nat la) (Nat ea)

data CreateLiquidityPoolParams = CreateLiquidityPoolParams
  { coinA           :: Coin,
    coinB           :: Coin,
    amountA         :: Integer,
    poolPartsParams :: CreatePoolPartsParams,
    swapFee         :: Percentage,
    exchangeRate    :: Percentage
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

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
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

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
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

convertPoolToPab :: CreatePoolPartsParams -> PoolPartsParams
convertPoolToPab (CreatePriceChangeParams (Percentage a) (Percentage b) np) =
  PriceChangeParams (pairFromDouble a) (pairFromDouble b) (Nat np)

convertPoolToMid :: PoolPartsParams -> CreatePoolPartsParams
convertPoolToMid (PriceChangeParams a b (Nat p)) =
  CreatePriceChangeParams (mkPercentage a) (mkPercentage b) p

newtype CancelOrderParams = CancelOrderParams { orderHash :: TxOutRef }
    deriving (Show, Generic)
    deriving anyclass (ToSchema, FromJSON, ToJSON)

newtype PerformRandomParams = PerformRandomParams { unPerformRandomParams :: Integer }
    deriving (Show, Generic)
    deriving anyclass (ToSchema, FromJSON, ToJSON)

-- VIEWS

-- | Data type to represent wallet founds.
data FundView = FundView
  { coin   :: Coin,
    amount :: Integer
  }
  deriving (Show, Generic, ToJSON, ToSchema)

mkFundView :: AssetClass -> Integer -> FundView
mkFundView = FundView . coinFromAssetClass

data OrderView = OrderView
  { orderHash    :: TxOutRef,
    lockedCoin   :: FundView,
    expectedCoin :: FundView,
    orderType    :: Text
  }
  deriving (Generic, Show, ToSchema, ToJSON)

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
  deriving (Show, Generic, ToJSON, ToSchema)

mkPayoutView :: AssetClass -> Integer -> PayoutView
mkPayoutView = PayoutView . coinFromAssetClass
