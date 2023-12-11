{-# LANGUAGE DerivingStrategies, DeriveAnyClass, GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings #-}

module Entities.Section where

import Database.PostgreSQL.Simple
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

getSection :: Connection -> Int64 -> IO[Section]
getSection conn id = query conn "SELECT * FROM sections WHERE id = ?" $ (Only id)

getAllSections :: Connection -> IO[Section]
getAllSections conn = query_ conn "SELECT * FROM sections"

createSection :: Connection -> String -> String -> IO[Only Int64]
createSection conn title description =
  query conn "INSERT INTO sections(title, description) \
              \ VALUES (?, ?) RETURNING id"
              $ ((title, description))

updateSectionTitle :: Connection -> Int64 -> String -> IO(Bool)
updateSectionTitle conn id title = do
  n <- execute conn "UPDATE sections SET title = ? WHERE id = ?" (title, id)
  return $ n > 0

updateSectionDescription :: Connection -> Int64 -> String -> IO(Bool)
updateSectionDescription conn id description = do
  n <- execute conn "UPDATE sections SET description = ? WHERE id = ?" (description, id)
  return $ n > 0

deleteSection :: Connection -> Int64 -> IO(Bool)
deleteSection conn id = do
  n <- execute conn "DELETE FROM sections WHERE id = ?" $ (Only id)
  return $ n > 0