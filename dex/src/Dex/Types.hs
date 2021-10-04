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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dex.Types
  where

import           Data.Aeson          (FromJSON (parseJSON), ToJSON)

import           Dex.WalletHistory
import           Ledger              (AssetClass, PubKeyHash, TxOutRef)
import           Playground.Contract (Generic, ToSchema)
import qualified PlutusTx
import           PlutusTx.Prelude    (AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, Integer, return, ($),
                                      (<))
import           Prelude             (Show)
import qualified Prelude


newtype Nat
  = Nat Integer
  deriving stock (Generic)
  deriving newtype
    ( AdditiveGroup
    , AdditiveMonoid
    , AdditiveSemigroup
    , Prelude.Enum
    , Prelude.Eq
    , Prelude.Integral
    , Prelude.Num
    , Prelude.Ord
    , Prelude.Real
    , Show
    , ToJSON
    , ToSchema
    )

fromNat :: Nat -> Integer
fromNat (Nat x) = x

PlutusTx.makeIsDataIndexed ''Nat [('Nat,0)]
PlutusTx.makeLift ''Nat


instance FromJSON Nat where
  parseJSON value = do
    integer <- parseJSON @Integer value
    if integer < 0 then
        Prelude.fail "parsing Natural failed, unexpected negative number "
    else
        return $ Nat integer


data DexAction = Perform | CancelOrder deriving (Show)

PlutusTx.makeIsDataIndexed
  ''DexAction
  [ ('Perform, 0)
  , ('CancelOrder, 1)
  ]
PlutusTx.makeLift ''DexAction


data SellOrderParams
  = SellOrderParams
      { coinIn         :: AssetClass
      , coinOut        :: AssetClass
      , amountIn       :: Nat
      , expectedAmount :: Nat
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

PlutusTx.makeIsDataIndexed ''SellOrderParams [('SellOrderParams,0)]
PlutusTx.makeLift ''SellOrderParams


data SellOrderInfo
  = SellOrderInfo
      { coinIn         :: AssetClass
      , coinOut        :: AssetClass
      , expectedAmount :: Nat
      , ownerHash      :: PubKeyHash
      }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)
PlutusTx.makeIsDataIndexed ''SellOrderInfo [('SellOrderInfo,0)]
PlutusTx.makeLift ''SellOrderInfo

newtype CancelOrderParams
  = CancelOrderParams { orderHash :: TxOutRef }
  deriving (FromJSON, Generic, Show, ToJSON, ToSchema)
PlutusTx.makeIsDataIndexed ''CancelOrderParams [('CancelOrderParams, 0)]
PlutusTx.makeLift ''CancelOrderParams

newtype DexDatum
  = SellOrder SellOrderInfo
  deriving stock (Generic, Show)
  deriving newtype (FromJSON, ToJSON, ToSchema)


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


data OrderInfo
  = OrderInfo
      { orderHash      :: TxOutRef
      , coinIn         :: AssetClass
      , coinOut        :: AssetClass
      , expectedAmount :: Nat
      , ownerHash      :: PubKeyHash
      }
  deriving (FromJSON, Generic, Show, ToJSON)

PlutusTx.makeIsDataIndexed ''OrderInfo [('OrderInfo, 0)]
PlutusTx.makeLift ''OrderInfo


data DexContractState
  = Orders [(SellOrderInfo, TxOutRef)]
  | Sold
  | Performed
  | Stopped
  | Funds [(AssetClass, Integer)]
  | MyOrders [OrderInfo]
  | Cancel
  deriving (FromJSON, Generic, Show, ToJSON)

PlutusTx.makeIsDataIndexed ''DexContractState
  [ ('Orders, 0)
  , ('Sold, 1)
  , ('Performed, 2)
  , ('Stopped, 3)
  , ('Funds, 4)
  , ('MyOrders, 5)
  , ('Cancel, 6)
  ]

PlutusTx.makeLift ''DexContractState
