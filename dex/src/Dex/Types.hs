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

module Dex.Types
  where

import           Control.Lens        ((^?!))
import           Data.Aeson          (FromJSON (parseJSON), ToJSON (toJSON),
                                      object, withObject, (.:), (.=))
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Extras   as JSON
import           Data.Aeson.Lens     (key)
import           Data.String         (IsString (fromString))
import qualified Data.Text.Encoding  as E
import           Dex.WalletHistory
import           Ledger              (AssetClass, CurrencySymbol, PubKeyHash,
                                      TokenName, TxOutRef, Value)
import           Ledger.Value        (AssetClass (..),
                                      CurrencySymbol (CurrencySymbol, unCurrencySymbol),
                                      assetClass, assetClassValue,
                                      assetClassValueOf, tokenName)
import           Playground.Contract (Generic, ToSchema)
import qualified PlutusTx
import           PlutusTx.Prelude    (AdditiveGroup, AdditiveMonoid,
                                      AdditiveSemigroup, Bool, ByteString,
                                      Eq (..), Integer, MultiplicativeSemigroup,
                                      Ord (max, min, (<=)), fst, return, snd,
                                      ($), (&&), (.), (||))
import           Prelude             (Show, show)
import qualified Prelude
import           Text.Printf         (PrintfArg)

data DexAction
  = Perform
  deriving (Show)

PlutusTx.makeIsDataIndexed
  ''DexAction
  [ ('Perform, 0)
  ]
PlutusTx.makeLift ''DexAction


data SellOrderParams
  = SellOrderParams
  { coinIn   :: AssetClass
  , coinOut  :: AssetClass
  , ratio    :: (Integer, Integer)
  , amountIn :: Integer
  } deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

PlutusTx.makeIsDataIndexed ''SellOrderParams [('SellOrderParams,0)]
PlutusTx.makeLift ''SellOrderParams


data SellOrderInfo
  = SellOrderInfo
  { coinIn    :: AssetClass
  , coinOut   :: AssetClass
  , ratio     :: (Integer, Integer)
  , ownerHash :: PubKeyHash
  } deriving (FromJSON, Generic, Show, ToJSON, ToSchema, Eq)
PlutusTx.makeIsDataIndexed ''SellOrderInfo [('SellOrderInfo,0)]
PlutusTx.makeLift ''SellOrderInfo


newtype DexDatum
  = SellOrder SellOrderInfo
  deriving stock (Show)
  deriving newtype (Eq)
  deriving (FromJSON, Generic, ToJSON, ToSchema)

PlutusTx.makeIsDataIndexed
  ''DexDatum
  [ ('SellOrder, 0)
  ]
PlutusTx.makeLift ''DexDatum


data WithHistoryId a
  = WithHistoryId
      { historyId :: HistoryId
      , content   :: a
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

data DexContractState
  = Orders [(SellOrderInfo, TxOutRef)]
  | Sold
  | Performed
  | Stopped
  | Funds [(AssetClass, Integer)]
  deriving (FromJSON, Generic, Show, ToJSON)
PlutusTx.makeIsDataIndexed ''DexContractState
  [ ('Orders,0),
    ('Sold,1),
    ('Performed, 2),
    ('Stopped, 3),
    ('Funds, 4)]
PlutusTx.makeLift ''DexContractState
