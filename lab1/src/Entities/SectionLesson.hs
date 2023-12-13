{-# LANGUAGE DerivingStrategies, DeriveAnyClass, GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings #-}

module Entities.SectionLesson where

import Database.PostgreSQL.Simple
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

getSectionLesson :: Connection -> Int64 -> IO[SectionLesson]
getSectionLesson conn id = query conn "SELECT * FROM sections_lessons WHERE id = ?" $ (Only id)

getAllSectionLessons :: Connection -> IO[SectionLesson]
getAllSectionLessons conn = query_ conn "SELECT * FROM sections_lessons"

createSectionLesson :: Connection -> Int64 -> Day -> TimeOfDay -> TimeOfDay -> IO[Only Int64]
createSectionLesson conn sectionId date startTime finishTime =
  query conn "INSERT INTO sections_lessons(section_id, date, start_time, finish_time) \
              \ VALUES (?, ?, ?, ?) RETURNING id"
              $ ((sectionId, date, startTime, finishTime))


deleteSectionLesson :: Connection -> Int64 -> IO(Bool)
deleteSectionLesson conn id = do
  n <- execute conn "DELETE FROM sections_lessons WHERE id = ?" $ (Only id)
  return $ n > 0
