{-# LANGUAGE DerivingStrategies, DeriveAnyClass, GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings #-}

module Entities.Student where

import Database.PostgreSQL.Simple
import Data.Time.Calendar.OrdinalDate
import Data.Time.LocalTime
import Data.Int(Int64)
import GHC.Generics(Generic)

data Student = Student {
    id :: Int64, -- corresponds to SERIAL type from PostgreSQL
    name :: String,
    surname :: String,
    patronymic :: String,
    birthday :: Day,
    address :: String,
    course :: Int
}
  deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)


getStudent :: Connection -> Int64 -> IO[Student]
getStudent conn id = query conn "SELECT * FROM students WHERE id = ?" $ (Only id)


getAllStudents :: Connection -> IO[Student]
getAllStudents conn = query_ conn "SELECT * FROM students"


createStudent :: Connection -> String -> String -> String -> Day -> String -> Int -> IO[Only Int64]
createStudent conn name surname patronymic birthday address course =
  query conn "INSERT INTO students(name, surname, patronymic, birthday, address, course) \
              \ VALUES (?, ?, ?, ?, ?, ?) RETURNING id"
              $ ((name, surname, patronymic, birthday, address, course))


updateStudentName :: Connection -> Int64 -> String -> IO(Bool)
updateStudentName conn id name = do
  n <- execute conn "UPDATE students SET name = ? WHERE id = ?" (name, id)
  return $ n > 0

updateStudentSurname :: Connection -> Int64 -> String -> IO(Bool)
updateStudentSurname conn id surname = do
  n <- execute conn "UPDATE students SET surname = ? WHERE id = ?" (surname, id)
  return $ n > 0

updateStudentPatronymic :: Connection -> Int64 -> String -> IO(Bool)
updateStudentPatronymic conn id patronymic = do
  n <- execute conn "UPDATE students SET patronymic = ? WHERE id = ?" (patronymic, id)
  return $ n > 0

updateStudentBirthday :: Connection -> Int64 -> Day -> IO(Bool)
updateStudentBirthday conn id birthday = do
  n <- execute conn "UPDATE students SET birthday = ? WHERE id = ?" (birthday, id)
  return $ n > 0

updateStudentAddress :: Connection -> Int64 -> String -> IO(Bool)
updateStudentAddress conn id address = do
  n <- execute conn "UPDATE students SET address = ? WHERE id = ?" (address, id)
  return $ n > 0

updateStudentCourse :: Connection -> Int64 -> Int -> IO(Bool)
updateStudentCourse conn id course = do
  n <- execute conn "UPDATE students SET course = ? WHERE id = ?" (course, id)
  return $ n > 0

deleteStudent :: Connection -> Int64 -> IO(Bool)
deleteStudent conn id = do
  n <- execute conn "DELETE FROM students WHERE id = ?" $ (Only id)
  return $ n > 0