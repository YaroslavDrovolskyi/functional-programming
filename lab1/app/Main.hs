{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import Database.PostgreSQL.Simple
import Entities.Student
import Menus.StudentMenu
import Menus.MainMenu
import Data.Time.Calendar.OrdinalDate
import Data.Time
import Data.Int
import Utils



retrieveBook :: Connection -> Int -> IO[(Int, String, Int, Int, Int)]
retrieveBook conn id = query conn "SELECT id, title, number_of_pages, publishing_year, quantity FROM books where id = ?" $ (Only id)




retrieveStudent :: Connection -> Int64 -> IO[Student]
retrieveStudent conn id = query conn "SELECT * FROM students WHERE id = ?" $ (Only id)

retrieveAllStudents :: Connection -> IO[Student]
retrieveAllStudents conn = query_ conn "SELECT * FROM students"

{-
createStudent :: Connection -> String -> String -> String -> String -> String -> Int -> IO[Only Int64]
createStudent conn name surname patronymic birthday address course =
  query conn "INSERT INTO students(name, surname, patronymic, birthday, address, course) \
              \ VALUES (?, ?, ?, ?, ?, ?) RETURNING id"
              $ ((name, surname, patronymic, birthday, address, course))

createStudent2 :: Connection -> String -> String -> String -> Day -> String -> Int -> IO[Only Int64]
createStudent2 conn name surname patronymic birthday address course =
  query conn "INSERT INTO students(name, surname, patronymic, birthday, address, course) \
              \ VALUES (?, ?, ?, ?, ?, ?) RETURNING id"
              $ ((name, surname, patronymic, birthday, address, course))
-}



main :: IO ()
main = do
  conn <- connectPostgreSQL "host='localhost' port=5432 dbname='faculty_sport' user='postgres' password='cnjrjl56cfgj;rb99'"

--  b <- updateStudentCourse conn 2 5
--  mapM_ print =<< (query_ conn "SELECT book_id FROM book_requests WHERE id=304" :: IO [Only Int])
--  mapM_ print =<< retrieveBook conn 104


--  mapM_ print =<< retrieveStudent conn 1
--  mapM_ print =<< retrieveAllStudents conn

--  query_ conn [sql| select 2+2 |] :: IO [Only Int]


  let dateString = "2017-12-09"
  let date = parseDate dateString
  b <- updateStudentBirthday conn 2 date

--  print $ timeFromString


--  studentMainMenu conn
  mainMenu conn


--  mapM_ print =<< createStudent2 conn "NedxvbfbwName" "cxbnmvnxzs" "NewPatrivxbfcnvgcbznymic" timeFromString "some dsfdgvaddress" 1
