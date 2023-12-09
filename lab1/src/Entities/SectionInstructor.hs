{-# LANGUAGE DerivingStrategies, DeriveAnyClass, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module Entities.SectionInstructor where

import Database.PostgreSQL.Simple(ToRow, FromRow)
import Data.Time.Calendar.OrdinalDate
import Data.Time.LocalTime
import Data.Int(Int64)
import GHC.Generics(Generic)

data SectionInstructor = SectionInstructor {
    id :: Int64,
    instructorId :: Int64,
    sectionId :: Int64
}
  deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)
