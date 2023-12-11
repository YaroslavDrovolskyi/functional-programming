module Menus.SectionMenu where


import System.IO
import Database.PostgreSQL.Simple
import Entities.Section
import Data.Int
import Utils


sectionMainMenu :: Connection -> IO()
sectionMainMenu conn = do
  putStrLn("")
  putStrLn("======================= Section menu =======================")
  putStrLn("  1) Show section")
  putStrLn("  2) Show all sections")
  putStrLn("  3) Add section")
  putStrLn("  4) Edit section")
  putStrLn("  5) Delete section")
  putStrLn("  6) Back")
  putStrLn("============================================================")
  putStr("Option: ")
  hFlush stdout

  resp <- getChar
  _ <- getLine

  case resp of
    '1' -> showSection conn
    '2' -> showAllSections conn
    '3' -> showCreateSection conn
    '4' -> showEditSection conn
    '5' -> showDeleteSection conn
    _ -> putStrLn("")

  if resp /= '6'
   then sectionMainMenu conn
  else putStr("")


showSection :: Connection -> IO()
showSection conn = do
  putStr("Enter ID: ")
  hFlush stdout

  input <- getLine
  let id = (read input :: Int64)

  result <- getSection conn id
  if null(result) == True
    then putStrLn("Section with such ID does not exist")
  else mapM_ print result


showAllSections :: Connection -> IO()
showAllSections conn = do
  result <- getAllSections conn
  if null(result) == True
    then putStrLn("No section found")
  else mapM_ print result


showCreateSection :: Connection -> IO()
showCreateSection conn = do
  putStr("Title: ")
  hFlush stdout
  title <- getLine

  putStr("Description (one line): ")
  hFlush stdout
  description <- getLine

  createSection conn title description
  putStr("")


showEditSection :: Connection -> IO()
showEditSection conn = do
  putStr("Enter Section ID: ")
  hFlush stdout

  idStr <- getLine
  let id = (read idStr :: Int64)

  section <- getSection conn id
  if null(section) == True
    then putStrLn("Section with such ID does not exist")
  else do
    mapM_ print section

    putStrLn("")
    putStrLn("What do you want to edit?")
    putStrLn("  1) title")
    putStrLn("  2) description")
    putStrLn("  3) [back]")
    putStr("Option: ")
    hFlush stdout

    resp <- getChar
    _ <- getLine

    case resp of
      '1' -> do
        putStr("Title: ")
        hFlush stdout
        title <- getLine

        res <- updateSectionTitle conn id title
        if res == True
          then putStrLn("Updated successfully")
        else putStrLn("Not updated")


      '2' -> do
        putStr("Description: ")
        hFlush stdout
        description <- getLine

        res <- updateSectionDescription conn id description
        if res == True
          then putStrLn("Updated successfully")
        else putStrLn("Not updated")

      _ -> putStrLn("")


showDeleteSection :: Connection -> IO()
showDeleteSection conn = do
  putStr("Enter Section ID: ")
  hFlush stdout

  idStr <- getLine
  let id = (read idStr :: Int64)

  section <- getSection conn id
  if null(section) == True
    then putStrLn("Section with such ID does not exist")
  else do
    mapM_ print section

    putStr("Do you want to delete? (y/n) ")
    hFlush stdout

    resp <- getChar
    _ <- getLine

    if resp == 'y'
      then do
        res <- deleteSection conn id
        if res == True
          then putStrLn("Deleted successfully!")
        else putStrLn("Not deleted")
    else putStrLn("")
