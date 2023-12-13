module Menus.InstructorMenu where

import System.IO
import Database.PostgreSQL.Simple
import Entities.Instructor
import Data.Int
import Utils



instructorMainMenu :: Connection -> IO()
instructorMainMenu conn = do
  putStrLn("")
  putStrLn("======================= Instructor menu =======================")
  putStrLn("  1) Show instructor")
  putStrLn("  2) Show all instructors")
  putStrLn("  3) Add instructor")
  putStrLn("  4) Edit instructor")
  putStrLn("  5) Delete instructor")
  putStrLn("  6) [Back]")
  putStrLn("============================================================")
  putStr("Option: ")
  hFlush stdout

  resp <- getChar
  _ <- getLine

  case resp of
    '1' -> showInstructor conn
    '2' -> showAllInstructors conn
    '3' -> showCreateInstructor conn
    '4' -> showEditInstructor conn
    '5' -> showDeleteInstructor conn
    _ -> putStrLn("")

  if resp /= '6'
   then instructorMainMenu conn
  else putStr("")


showInstructor :: Connection -> IO()
showInstructor conn = do
  putStr("Enter ID: ")
  hFlush stdout

  input <- getLine
  let id = (read input :: Int64)

  result <- getInstructor conn id
  if null(result) == True
    then putStrLn("Instructor with such ID does not exist")
  else mapM_ print result


showAllInstructors :: Connection -> IO()
showAllInstructors conn = do
  result <- getAllInstructors conn
  if null(result) == True
    then putStrLn("No instructor found")
  else mapM_ print result


showCreateInstructor :: Connection -> IO()
showCreateInstructor conn = do
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

  putStr("Degree: ")
  hFlush stdout
  degree <- getLine

  createInstructor conn name surname patronymic birthday degree
  putStr("")


showEditInstructor :: Connection -> IO()
showEditInstructor conn = do
  putStr("Enter Instructor ID: ")
  hFlush stdout

  idStr <- getLine
  let id = (read idStr :: Int64)

  instructor <- getInstructor conn id
  if null(instructor) == True
    then putStrLn("Instructor with such ID does not exist")
  else do
    mapM_ print instructor

    putStrLn("")
    putStrLn("What do you want to edit?")
    putStrLn("  1) name")
    putStrLn("  2) surname")
    putStrLn("  3) patronymic")
    putStrLn("  4) birthday")
    putStrLn("  5) degree")
    putStrLn("  6) [back]")
    putStr("Option: ")
    hFlush stdout

    resp <- getChar
    _ <- getLine

    case resp of
      '1' -> do
        putStr("Name: ")
        hFlush stdout
        name <- getLine

        res <- updateInstructorName conn id name
        if res == True
          then putStrLn("Updated successfully")
        else putStrLn("Not updated")


      '2' -> do
        putStr("Surname: ")
        hFlush stdout
        surname <- getLine

        res <- updateInstructorSurname conn id surname
        if res == True
          then putStrLn("Updated successfully")
        else putStrLn("Not updated")


      '3' -> do
        putStr("Patronymic: ")
        hFlush stdout
        patronymic <- getLine

        res <- updateInstructorPatronymic conn id patronymic
        if res == True
          then putStrLn("Updated successfully")
        else putStrLn("Not updated")


      '4' -> do
        putStr("Birthday (yyyy-mm-dd): ")
        hFlush stdout
        birthdayStr <- getLine
        let birthday = parseDate birthdayStr

        res <- updateInstructorBirthday conn id birthday
        if res == True
          then putStrLn("Updated successfully")
        else putStrLn("Not updated")


      '5' -> do
        putStr("Degree: ")
        hFlush stdout
        degree <- getLine

        res <- updateInstructorDegree conn id degree
        if res == True
          then putStrLn("Updated successfully")
        else putStrLn("Not updated")


      _ -> putStrLn("")


showDeleteInstructor :: Connection -> IO()
showDeleteInstructor conn = do
  putStr("Enter Instructor ID: ")
  hFlush stdout

  idStr <- getLine
  let id = (read idStr :: Int64)

  instructor <- getInstructor conn id
  if null(instructor) == True
    then putStrLn("Instructor with such ID does not exist")
  else do
    mapM_ print instructor

    putStr("Do you want to delete? (y/n) ")
    hFlush stdout

    resp <- getChar
    _ <- getLine

    if resp == 'y'
      then do
        res <- deleteInstructor conn id
        if res == True
          then putStrLn("Deleted successfully!")
        else putStrLn("Not deleted")
    else putStrLn("")


