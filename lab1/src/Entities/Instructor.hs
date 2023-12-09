{-# LANGUAGE DerivingStrategies, DeriveAnyClass, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module Entities.Instructor where

import Database.PostgreSQL.Simple(ToRow, FromRow)
import Data.Time.Calendar.OrdinalDate
import Data.Time.LocalTime
import Data.Int(Int64)
import GHC.Generics(Generic)

data Instructor = Instructor {
    id :: Int64,
    name :: String,
    surname :: String,
    patronymic :: String,
    birthday :: Day,
    degree :: String
}
  deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)