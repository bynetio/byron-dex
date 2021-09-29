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
import           PlutusTx.Prelude    (AdditiveGroup, AdditiveMonoid,
                                      AdditiveSemigroup, Integer, return, ($),
                                      (<))
import           Prelude             (Show)
import qualified Prelude




newtype Nat = Nat Integer
  deriving stock (Generic)
  deriving newtype
  ( Show
  , AdditiveGroup
  , AdditiveMonoid
  , AdditiveSemigroup
  , ToSchema
  , ToJSON
  , Prelude.Integral
  , Prelude.Real
  , Prelude.Enum
  , Prelude.Num
  , Prelude.Ord
  , Prelude.Eq
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
  , ratio    :: (Nat, Nat)
  , amountIn :: Integer
  } deriving (FromJSON, Generic, Show, ToJSON, ToSchema)

PlutusTx.makeIsDataIndexed ''SellOrderParams [('SellOrderParams,0)]
PlutusTx.makeLift ''SellOrderParams


data SellOrderInfo
  = SellOrderInfo
  { coinIn    :: AssetClass
  , coinOut   :: AssetClass
  , ratio     :: (Nat, Nat)
  , ownerHash :: PubKeyHash
  } deriving (FromJSON, Generic, Show, ToJSON, ToSchema)
PlutusTx.makeIsDataIndexed ''SellOrderInfo [('SellOrderInfo,0)]
PlutusTx.makeLift ''SellOrderInfo


newtype DexDatum
  = SellOrder SellOrderInfo
  deriving stock (Show, Generic)
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
