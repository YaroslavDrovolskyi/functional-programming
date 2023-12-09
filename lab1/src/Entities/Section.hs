{-# LANGUAGE DerivingStrategies, DeriveAnyClass, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module Entities.Section where

import Database.PostgreSQL.Simple(ToRow, FromRow)
import Data.Time.Calendar.OrdinalDate
import Data.Time.LocalTime
import Data.Int(Int64)
import GHC.Generics(Generic)

data Section = Section {
    id :: Int64,
    title :: String,
    description :: String
}
  deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)