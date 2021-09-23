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
import           Ledger              (AssetClass, CurrencySymbol, PubKeyHash,
                                      TokenName, Value)
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
  } deriving (FromJSON, Generic, Show, ToJSON, ToSchema)
PlutusTx.makeIsDataIndexed ''SellOrderInfo [('SellOrderInfo,0)]
PlutusTx.makeLift ''SellOrderInfo


newtype DexDatum
  = SellOrder SellOrderInfo
  deriving stock (Show)

PlutusTx.makeIsDataIndexed
  ''DexDatum
  [ ('SellOrder, 0)
  ]
PlutusTx.makeLift ''DexDatum
