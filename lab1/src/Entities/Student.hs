{-# LANGUAGE DerivingStrategies, DeriveAnyClass, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module Entities.Student where

import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Data.Time.Calendar.OrdinalDate

data Student = Student {
    id :: Int,
    name :: String,
    surname :: String,
    patronymic :: String,
    birthday :: Day,
    address :: String,
    course :: Int
}
  deriving (Show)
--  deriving anyclass (ToRow, FromRow)

instance FromRow Student where
  fromRow = Student <$> field <*> field <*> field <*> field <*> field <*> field <*> field