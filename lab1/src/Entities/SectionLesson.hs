{-# LANGUAGE DerivingStrategies, DeriveAnyClass, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module Entities.SectionLesson where

import Database.PostgreSQL.Simple(ToRow, FromRow)
import Data.Time.Calendar.OrdinalDate
import Data.Time.LocalTime
import Data.Int(Int64)
import GHC.Generics(Generic)

data SectionLesson = SectionLesson {
    id :: Int64,
    sectionId :: Int64,
    date :: Day,
    startTime :: TimeOfDay,
    finishTime :: TimeOfDay
}
  deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)
