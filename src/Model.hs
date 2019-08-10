{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE QuasiQuotes                #-}
module Model where

import RIO
import Data.Time.Clock
import Database.Persist.TH
import Lib.Core.Types

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
OpenTrade json
    asset Asset
    entranceTime UTCTime
    entrancePrice Price
    amount Amount
    deriving Eq
    deriving Show

TradeRecord json
    asset Asset
    entranceTime UTCTime
    entrancePrice Price
    amount Amount
    exitTime UTCTime
    exitPrice Price
    deriving Eq
    deriving Show

BinanceHistoricalPrices1H json
    asset Asset
    time UTCTime
    open Price
    high Price
    low Price
    close Price

    UniqueOpenTime time
    deriving Eq
    deriving Show

BtcPriceRecords json
    createdAt UTCTime
    btcUsd Price
|]

type TradePool = [OpenTrade]
type InputTrades = [OpenTrade]
type OutputTrades = (TradeRecord, [OpenTrade])
