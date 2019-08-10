{-# LANGUAGE TemplateHaskell #-}
module Lib.Core.Types where

import RIO

import Data.Aeson
import Data.Scientific
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH

newtype Price = Price { unPrice :: Scientific }
    deriving (Eq, Ord, Num, Show, ToJSON, FromJSON)
instance PersistField Price where
    toPersistValue = PersistRational . toRational . unPrice
    fromPersistValue (PersistRational r) = Right . Price $ fromRational r
    fromPersistValue _ = Left "Expected Rational Numeric Type"
instance PersistFieldSql Price where
    sqlType _ = SqlNumeric 32 18

newtype Amount = Amount { unAmount :: Scientific }
    deriving (Eq, Ord, Num, Show, ToJSON, FromJSON)
instance PersistField Amount where
    toPersistValue = PersistRational . toRational . unAmount
    fromPersistValue (PersistRational r) = Right . Amount $ fromRational r
    fromPersistValue _ = Left "Expected Rational Numeric Type"
instance PersistFieldSql Amount where
    sqlType _ = SqlNumeric 32 18

data Asset = BTC | ETH | XMR | ADA deriving (Eq, Show, Read, Generic)
derivePersistField "Asset"
instance ToJSON Asset
instance FromJSON Asset
