{-# LANGUAGE DerivingStrategies, DeriveAnyClass, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module Entities.Competition where

import Database.PostgreSQL.Simple(ToRow, FromRow)
import Data.Time.Calendar.OrdinalDate
import Data.Time.LocalTime
import Data.Int(Int64)
import GHC.Generics(Generic)

data Competition = Competition {
    id :: Int64,
    sectionId :: Int64,
    startTimestamp :: LocalTime,
    venue :: String
}
  deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)