{-# LANGUAGE DerivingStrategies, DeriveAnyClass, GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings #-}

module Entities.Instructor where

import Database.PostgreSQL.Simple
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

getInstructor :: Connection -> Int64 -> IO[Instructor]
getInstructor conn id = query conn "SELECT * FROM instructors WHERE id = ?" $ (Only id)

getAllInstructors :: Connection -> IO[Instructor]
getAllInstructors conn = query_ conn "SELECT * FROM instructors"

createInstructor :: Connection -> String -> String -> String -> Day -> String -> IO[Only Int64]
createInstructor conn name surname patronymic birthday degree =
  query conn "INSERT INTO instructors(name, surname, patronymic, birthday, degree) \
              \ VALUES (?, ?, ?, ?, ?) RETURNING id"
              $ ((name, surname, patronymic, birthday, degree))

updateInstructorName :: Connection -> Int64 -> String -> IO(Bool)
updateInstructorName conn id name = do
  n <- execute conn "UPDATE instructors SET name = ? WHERE id = ?" (name, id)
  return $ n > 0

updateInstructorSurname :: Connection -> Int64 -> String -> IO(Bool)
updateInstructorSurname conn id surname = do
  n <- execute conn "UPDATE instructors SET surname = ? WHERE id = ?" (surname, id)
  return $ n > 0

updateInstructorPatronymic :: Connection -> Int64 -> String -> IO(Bool)
updateInstructorPatronymic conn id patronymic = do
  n <- execute conn "UPDATE instructors SET patronymic = ? WHERE id = ?" (patronymic, id)
  return $ n > 0

updateInstructorBirthday :: Connection -> Int64 -> Day -> IO(Bool)
updateInstructorBirthday conn id birthday = do
  n <- execute conn "UPDATE instructors SET birthday = ? WHERE id = ?" (birthday, id)
  return $ n > 0

updateInstructorDegree :: Connection -> Int64 -> String -> IO(Bool)
updateInstructorDegree conn id degree = do
  n <- execute conn "UPDATE instructors SET degree = ? WHERE id = ?" (degree, id)
  return $ n > 0

deleteInstructor :: Connection -> Int64 -> IO(Bool)
deleteInstructor conn id = do
  n <- execute conn "DELETE FROM instructors WHERE id = ?" $ (Only id)
  return $ n > 0
