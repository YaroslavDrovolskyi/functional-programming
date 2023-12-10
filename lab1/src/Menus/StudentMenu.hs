module Menus.StudentMenu where


import System.IO
import Database.PostgreSQL.Simple
import Entities.Student
import Data.Int
import Utils




studentMainMenu :: Connection -> IO()
studentMainMenu conn = do
  putStrLn("")
  putStrLn("======================= Student menu =======================")
  putStrLn("  1) Show student")
  putStrLn("  2) Show all students")
  putStrLn("  3) Add student")
  putStrLn("  4) Edit student")
  putStrLn("  5) Delete student")
  putStrLn("  6) Back")
  putStrLn("============================================================")
  putStr("Option: ")
  hFlush stdout

  resp <- getChar
  _ <- getLine

  case resp of
    '1' -> showStudent conn
    '2' -> showAllStudents conn
    '3' -> showCreateStudent conn
    '4' -> showEditStudent conn
    '5' -> showEditStudent conn
    _ -> putStrLn("")

  if resp /= '6'
   then studentMainMenu conn
  else putStr("")


showStudent :: Connection -> IO()
showStudent conn = do
  putStr("Enter ID: ")
  hFlush stdout

  input <- getLine
  let id = (read input :: Int64)

  result <- getStudent conn id
  if null(result) == True
    then putStrLn("Student with such ID does not exist")
  else mapM_ print result


showAllStudents :: Connection -> IO()
showAllStudents conn = do
  result <- getAllStudents conn
  if null(result) == True
    then putStrLn("No student found")
  else mapM_ print result


showCreateStudent :: Connection -> IO()
showCreateStudent conn = do
  putStr("Name: ")
  hFlush stdout
  name <- getLine

  putStr("Surname: ")
  hFlush stdout
  surname <- getLine

  putStr("Patronymic: ")
  hFlush stdout
  patronymic <- getLine

  putStr("Birthday (yyyy-mm-dd): ")
  hFlush stdout
  birthdayStr <- getLine
  let birthday = parseDate birthdayStr

  putStr("Address: ")
  hFlush stdout
  address <- getLine

  putStr("Course: ")
  hFlush stdout
  courseStr <- getLine
  let course = (read courseStr :: Int)

  createStudent conn name surname patronymic birthday address course
  putStr("")


showEditStudent :: Connection -> IO()
showEditStudent conn = do
  putStr("Enter Student ID: ")
  hFlush stdout

  idStr <- getLine
  let id = (read idStr :: Int64)

  student <- getStudent conn id
  if null(student) == True
    then putStrLn("Student with such ID does not exist")
  else do
    mapM_ print student

    putStrLn("")
    putStrLn("What do you want to edit?")
    putStrLn("  1) name")
    putStrLn("  2) surname")
    putStrLn("  3) patronymic")
    putStrLn("  4) birthday")
    putStrLn("  5) address")
    putStrLn("  6) course")
    putStrLn("  7) [back]")
    putStr("Option: ")
    hFlush stdout

    resp <- getChar
    _ <- getLine

    case resp of
      '1' -> do
        putStr("Name: ")
        hFlush stdout
        name <- getLine

        res <- updateStudentName conn id name
        if res == True
          then putStrLn("Updated successfully")
        else putStrLn("Not updated")


      '2' -> do
        putStr("Surname: ")
        hFlush stdout
        surname <- getLine

        res <- updateStudentSurname conn id surname
        if res == True
          then putStrLn("Updated successfully")
        else putStrLn("Not updated")


      '3' -> do
        putStr("Patronymic: ")
        hFlush stdout
        patronymic <- getLine

        res <- updateStudentPatronymic conn id patronymic
        if res == True
          then putStrLn("Updated successfully")
        else putStrLn("Not updated")


      '4' -> do
        putStr("Birthday (yyyy-mm-dd): ")
        hFlush stdout
        birthdayStr <- getLine
        let birthday = parseDate birthdayStr

        res <- updateStudentBirthday conn id birthday
        if res == True
          then putStrLn("Updated successfully")
        else putStrLn("Not updated")


      '5' -> do
        putStr("Address: ")
        hFlush stdout
        address <- getLine

        res <- updateStudentAddress conn id address
        if res == True
          then putStrLn("Updated successfully")
        else putStrLn("Not updated")


      '6' -> do
        putStr("Course: ")
        hFlush stdout
        courseStr <- getLine
        let course = (read courseStr :: Int)

        res <- updateStudentCourse conn id course
        if res == True
          then putStrLn("Updated successfully")
        else putStrLn("Not updated")


      _ -> putStrLn("")


