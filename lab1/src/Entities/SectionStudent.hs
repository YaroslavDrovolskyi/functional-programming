{-# LANGUAGE DerivingStrategies, DeriveAnyClass, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module Entities.SectionStudent where

import Database.PostgreSQL.Simple(ToRow, FromRow)
import Data.Time.Calendar.OrdinalDate
import Data.Time.LocalTime
import Data.Int(Int64)
import GHC.Generics(Generic)

data SectionStudent = SectionStudent {
    id :: Int64,
    studentId :: Int64,
    sectionId :: Int64
}
  deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)