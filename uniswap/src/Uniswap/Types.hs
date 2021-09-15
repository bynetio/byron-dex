{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Uniswap.Types
  where

import           Control.Lens                 ((^?!))
import           Data.Aeson                   (FromJSON (parseJSON), ToJSON (toJSON), object, withObject,
                                               (.:), (.=))
import qualified Data.Aeson                   as JSON
import qualified Data.Aeson.Extras            as JSON
import           Data.Aeson.Lens              (key)
import           Data.String                  (IsString (fromString))
import qualified Data.Text.Encoding           as E
import           Ledger                       (AssetClass, CurrencySymbol, TokenName, Value)
import           Ledger.Value                 (AssetClass (..),
                                               CurrencySymbol (CurrencySymbol, unCurrencySymbol), assetClass,
                                               assetClassValue, assetClassValueOf, tokenName)
import           Playground.Contract          (Generic, ToSchema)
import qualified PlutusTx
import           PlutusTx.Prelude             (AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, Bool,
                                               ByteString, Eq (..), Integer, MultiplicativeSemigroup,
                                               Ord (max, min, (<=)), fst, return, snd, ($), (&&), (.), (||))
import           Prelude                      (Show, show)
import qualified Prelude
import           Text.Printf                  (PrintfArg)
import           Uniswap.Common.WalletHistory (HistoryId)

type Fee = (Integer, Integer)

-- | Uniswap coin token
data U = U

PlutusTx.makeIsDataIndexed ''U [('U, 0)]
PlutusTx.makeLift ''U

-- | "A"-side coin token
data A = A

PlutusTx.makeIsDataIndexed ''A [('A, 0)]
PlutusTx.makeLift ''A

-- | "B"-side coin token
data B = B

PlutusTx.makeIsDataIndexed ''B [('B, 0)]
PlutusTx.makeLift ''B

-- | Pool-state coin token
data PoolState = PoolState

PlutusTx.makeIsDataIndexed ''PoolState [('PoolState, 0)]
PlutusTx.makeLift ''PoolState

-- | Liquidity-state coin token
data Liquidity = Liquidity

PlutusTx.makeIsDataIndexed ''Liquidity [('Liquidity, 0)]
PlutusTx.makeLift ''Liquidity

-- | A single 'AssetClass'. Because we use three coins, we use a phantom type to track
-- which one is which.
newtype Coin a
  = Coin { unCoin :: AssetClass }
  deriving stock (Generic, Show)
  deriving newtype (Eq, Prelude.Eq, Prelude.Ord, ToSchema)

instance ToJSON (Coin a) where
  toJSON coin =
    object
      [ "currencySymbol" .= encodeCoin (JSON.encodeByteString . unCurrencySymbol . fst),
        "tokenName" .= (toJSON (snd $ unAssetClass $ unCoin coin) ^?! key "unTokenName")
      ]
    where
      encodeCoin f = JSON.String . f . unAssetClass . unCoin $ coin

instance FromJSON (Coin a) where
  parseJSON = withObject "Coin" $ \v -> do
    rawTokenName <- v .: "tokenName"
    rawCurrencySymbol <- v .: "currencySymbol"
    currencySymbolBytes <- JSON.decodeByteString rawCurrencySymbol
    let currencySymbol' = CurrencySymbol currencySymbolBytes
        tokenName' = tokenName . E.encodeUtf8 $ rawTokenName
    return . Coin $ assetClass currencySymbol' tokenName'

PlutusTx.makeIsDataIndexed ''Coin [('Coin, 0)]
PlutusTx.makeLift ''Coin

-- | Likewise for 'Integer'; the corresponding amount we have of the
-- particular 'Coin'.
newtype Amount a
  = Amount { unAmount :: Integer }
  deriving stock (Generic, Show)
  deriving newtype (Eq, FromJSON, Ord, PrintfArg, ToJSON, ToSchema)
  deriving newtype (Prelude.Enum, Prelude.Eq, Prelude.Integral, Prelude.Num, Prelude.Ord, Prelude.Real)
  deriving newtype (AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, MultiplicativeSemigroup)

PlutusTx.makeIsDataIndexed ''Amount [('Amount, 0)]
PlutusTx.makeLift ''Amount

data AmountOfCoin a
  = AmountOfCoin
      { coin   :: Coin a
      , amount :: Amount a
      }
  deriving (Eq, FromJSON, Generic, Show, ToJSON, ToSchema)

{-# INLINEABLE valueOf #-}
valueOf :: Coin a -> Amount a -> Value
valueOf c a = assetClassValue (unCoin c) (unAmount a)

{-# INLINEABLE unitValue #-}
unitValue :: Coin a -> Value
unitValue c = valueOf c 1

{-# INLINEABLE isUnity #-}
isUnity :: Value -> Coin a -> Bool
isUnity v c = amountOf v c == 1

{-# INLINEABLE amountOf #-}
amountOf :: Value -> Coin a -> Amount a
amountOf v = Amount . assetClassValueOf v . unCoin

{-# INLINEABLE mkCoin #-}
mkCoin :: CurrencySymbol -> TokenName -> Coin a
mkCoin c = Coin . assetClass c

-- | Wraps uniswap NFT Token
newtype Uniswap
  = Uniswap { usCoin :: Coin U }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving newtype (Prelude.Eq, Prelude.Ord)

PlutusTx.makeIsDataIndexed ''Uniswap [('Uniswap, 0)]
PlutusTx.makeLift ''Uniswap

data LiquidityPool
  = LiquidityPool
      { lpCoinA         :: Coin A
      , lpCoinB         :: Coin B
      , lpFee           :: Fee
      , lpFeeByteString :: ByteString
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

PlutusTx.makeIsDataIndexed ''LiquidityPool [('LiquidityPool, 0)]
PlutusTx.makeLift ''LiquidityPool

liquidityPool :: (Coin A, Coin B) -> Fee -> LiquidityPool
liquidityPool (Coin a, Coin b) fee = LiquidityPool (Coin (min a b)) (Coin (max a b)) fee $ fromString $ show fee

data LiquidityPoolWithCoins
  = LiquidityPoolWithCoins
      { coinA         :: Coin A
      , coinB         :: Coin B
      , fee           :: Fee
      , amountA       :: Amount A
      , amountB       :: Amount B
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

data LiquidityPoolView
  = LiquidityPoolView
      { coinA         :: Coin A
      , coinB         :: Coin B
      , fee           :: Fee
      , amountA       :: Amount A
      , amountB       :: Amount B
      , liquidityCoin :: Coin Liquidity
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

-- | just swap in place the coins not touching types
swapCoins :: (Coin A, Coin B) -> (Coin A, Coin B)
swapCoins (ca, cb) = (Coin $ unCoin cb, Coin $ unCoin ca)

instance Eq LiquidityPool where
  {-# INLINEABLE (==) #-}
  x == y = cmpFee && cmpCoins (lpCoinA x, lpCoinB x) (lpCoinA y, lpCoinB y)
    where
      cmpFee :: Bool
      cmpFee = lpFee x == lpFee y && lpFeeByteString x == lpFeeByteString y

      cmpCoins :: (Coin A, Coin B) -> (Coin A, Coin B) -> Bool
      cmpCoins lp1 lp2 = lp1 == lp2 || lp1 == swapCoins lp2

instance Prelude.Eq LiquidityPool where
  x == y = plutusEq x y
    where
      plutusEq :: Eq s => s -> s -> Bool
      plutusEq s1 s2 = s1 == s2

instance Prelude.Ord LiquidityPool where
  compare (LiquidityPool a b fee fbs) (LiquidityPool a2 b2 fee2 fbs2) =
    let lp1 = order (a, b)
        lp2 = order (a2, b2)
     in Prelude.compare (lp1, fee, fbs) (lp2, fee2, fbs2)
    where
      order :: (Coin A, Coin B) -> (Coin A, Coin B)
      order (ca, cb) = if unCoin ca <= unCoin cb then (ca, cb) else swapCoins (ca, cb)

data UniswapAction
  = Create LiquidityPool
  | Close
  | Swap
  | ISwap
  | Remove
  | Add
  deriving (Show)

PlutusTx.makeIsDataIndexed
  ''UniswapAction
  [ ('Create, 0),
    ('Close, 1),
    ('Swap, 2),
    ('ISwap, 3),
    ('Remove, 4),
    ('Add, 5)
  ]
PlutusTx.makeLift ''UniswapAction

-- | UniswapDatum keeps track of all available liquidity pools @Factory [LiquidityPool]@ or describes a given liquidity pool
-- by coins pair @LiquidityPool@ and amount of liquidity using @Pool LiquidityPool (Amount Liquidity)@ data cosntructor
data UniswapDatum
  = Factory [LiquidityPool]
  | Pool LiquidityPool (Amount Liquidity)
  deriving stock (Show)

PlutusTx.makeIsDataIndexed
  ''UniswapDatum
  [ ('Factory, 0),
    ('Pool, 1)
  ]
PlutusTx.makeLift ''UniswapDatum

-- | Parameters for the @create@-endpoint, which creates a new liquidity pool.
data CreateParams
  = CreateParams
      { -- | One 'Coin' of the liquidity pair.
        coinA   :: Coin A
        -- | The other 'Coin'.
      , coinB   :: Coin B
        -- | Numerator and denominator of the swap fee
      , fee     :: Fee
        -- | Amount of liquidity for the first 'Coin'.
      , amountA :: Amount A
        -- | Amount of liquidity for the second 'Coin'.
      , amountB :: Amount B
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

-- | Parameters for the @swap@-endpoint, which allows swaps between the two different coins in a liquidity pool.
-- One of the provided amounts must be positive, the other must be zero.
data SwapParams
  = SwapParams
      { -- | One 'Coin' of the liquidity pair.
        coinA    :: Coin A
        -- | The other 'Coin'.
      , coinB    :: Coin B
        -- | Numerator and denominator of the swap fee
      , fee      :: Fee
        -- | The amount the first 'Coin' that should be swapped.
      , amount   :: Amount A
        -- | The expected amount of swaped 'Coin B' (quoted amount)
      , result   :: Amount B
        -- | The expected % difference between quoted and executed prices.
      , slippage :: Integer
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

data SwapPreviewParams
  = SwapPreviewParams
      { coinA  :: Coin A
      , coinB  :: Coin B
        -- | Numerator and denominator of the swap fee
      , fee    :: Fee
      , amount :: Amount A
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

data IndirectSwapParams
  = IndirectSwapParams
      { -- | One 'Coin' of the liquidity pair.
        coinA    :: Coin A
        -- | The other 'Coin'.
      , coinB    :: Coin B
        -- | The amount of the first 'Coin' that should be swapped.
      , amount   :: Amount A
      , result   :: Amount B
      , slippage :: Integer
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

data ISwapPreviewParams
  = ISwapPreviewParams
      { coinA  :: Coin A
      , coinB  :: Coin B
      , amount :: Amount A
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

-- | Parameters for the @close@-endpoint, which closes a liquidity pool.
data CloseParams
  = CloseParams
      { -- | One 'Coin' of the liquidity pair.
        coinA :: Coin A
        -- | The other 'Coin' of the liquidity pair.
      , coinB :: Coin B
        -- | Numerator and denominator of the swap fee
      , fee   :: Fee
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

-- | Parameters for the @remove@-endpoint, which removes some liquidity from a liquidity pool.
data RemoveParams
  = RemoveParams
      { -- | One 'Coin' of the liquidity pair.
        coinA :: Coin A
        -- | The other 'Coin' of the liquidity pair.
      , coinB :: Coin B
        -- | Numerator and denominator of the swap fee
      , fee   :: Fee
        -- | The amount of liquidity tokens to burn in exchange for liquidity from the pool.
      , diff  :: Amount Liquidity
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

-- | Parameters for the @add@-endpoint, which adds liquidity to a liquidity pool in exchange for liquidity tokens.
data AddParams
  = AddParams
      { -- | One 'Coin' of the liquidity pair.
        coinA   :: Coin A
        -- | The other 'Coin' of the liquidity pair.
      , coinB   :: Coin B
        -- | Numerator and denominator of the swap fee
      , fee     :: Fee
        -- | The amount of coins of the first kind to add to the pool.
      , amountA :: Amount A
        -- | The amount of coins of the second kind to add to the pool.
      , amountB :: Amount B
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

-- | Parameters for the @clearState-@endpoint, which removes entry from the state corresponding to given HistoryId
newtype ClearStateParams
  = ClearStateParams { removeId :: HistoryId }
  -- | Identifier of Operation that should be removed from state
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

data SwapPreviewResultData
  = SwapPreviewResultData
      { coinA   :: Coin A
      , amountA :: Amount A
      , coinB   :: Coin B
      , amountB :: Amount B
      , fee     :: Fee
      }
  deriving (FromJSON, Generic, Show, ToJSON)

data ISwapPreviewResultData
  = ISwapPreviewResultData
      { coinA   :: Coin A
      , amountA :: Amount A
      , coinB   :: Coin B
      , amountB :: Amount B
      }
  deriving (FromJSON, Generic, Show, ToJSON)

data WithHistoryId a
  = WithHistoryId
      { historyId :: HistoryId
      , content   :: a
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)
