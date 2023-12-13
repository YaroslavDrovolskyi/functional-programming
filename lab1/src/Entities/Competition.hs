{-# LANGUAGE DerivingStrategies, DeriveAnyClass, GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings #-}

module Entities.Competition where

import Database.PostgreSQL.Simple
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

getCompetition :: Connection -> Int64 -> IO[Competition]
getCompetition conn id = query conn "SELECT * FROM competitions WHERE id = ?" $ (Only id)

getAllCompetitions :: Connection -> IO[Competition]
getAllCompetitions conn = query_ conn "SELECT * FROM competitions"

createCompetition :: Connection -> Int64 -> LocalTime -> String -> IO[Only Int64]
createCompetition conn sectionId startTimestamp venue =
  query conn "INSERT INTO competitions(section_id, start_timestamp, venue) \
              \ VALUES (?, ?, ?) RETURNING id"
              $ ((sectionId, startTimestamp, venue))

updateCompetitionStartTimestamp :: Connection -> Int64 -> LocalTime -> IO(Bool)
updateCompetitionStartTimestamp conn id startTimestamp = do
  n <- execute conn "UPDATE competitions SET start_timestamp = ? WHERE id = ?" (startTimestamp, id)
  return $ n > 0

updateCompetitionVenue :: Connection -> Int64 -> String -> IO(Bool)
updateCompetitionVenue conn id venue = do
  n <- execute conn "UPDATE competitions SET venue = ? WHERE id = ?" (venue, id)
  return $ n > 0

deleteCompetition :: Connection -> Int64 -> IO(Bool)
deleteCompetition conn id = do
  n <- execute conn "DELETE FROM competitions WHERE id = ?" $ (Only id)
  return $ n > 0

getAllCompetitionsForSection :: Connection -> Int64 -> IO[Competition]
getAllCompetitionsForSection conn sectionId = query conn "SELECT * FROM competitions WHERE section_id = ?" $ (Only sectionId)


