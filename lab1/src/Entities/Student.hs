{-# LANGUAGE DerivingStrategies, DeriveAnyClass, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module Entities.Student where

import Database.PostgreSQL.Simple(ToRow, FromRow)
import Data.Time.Calendar.OrdinalDate
import Data.Time.LocalTime
import Data.Int(Int64)
import GHC.Generics(Generic)

data Student = Student {
    id :: Int64, -- corresponds to SERIAL type from PostgreSQL
    name :: String,
    surname :: String,
    patronymic :: String,
    birthday :: Day,
    address :: String,
    course :: Int
}
  deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)