module Menus.CompetitionMenu where


import System.IO
import Database.PostgreSQL.Simple
import Entities.Competition
import Entities.Section
import Data.Int
import Utils


competitionMainMenu :: Connection -> IO()
competitionMainMenu conn = do
  putStrLn("")
  putStrLn("======================= Competition menu =======================")
  putStrLn("  1) Show competition")
  putStrLn("  2) Show all competitions")
  putStrLn("  3) Show all competitions")
  putStrLn("  4) Add competition")
  putStrLn("  5) Edit competition")
  putStrLn("  6) Delete competition")
  putStrLn("  7) Back")
  putStrLn("============================================================")
  putStr("Option: ")
  hFlush stdout

  resp <- getChar
  _ <- getLine

  case resp of
    '1' -> showCompetition conn
    '2' -> showAllCompetitions conn
    '3' -> showAllCompetitionsForSection conn
    '4' -> showCreateCompetition conn
    '5' -> showEditCompetition conn
    '6' -> showDeleteCompetition conn
    _ -> putStrLn("")

  if resp /= '7'
   then competitionMainMenu conn
  else putStr("")


showCompetition :: Connection -> IO()
showCompetition conn = do
  putStr("Enter ID: ")
  hFlush stdout

  input <- getLine
  let id = (read input :: Int64)

  result <- getCompetition conn id
  if null(result) == True
    then putStrLn("Competition with such ID does not exist")
  else mapM_ print result


showAllCompetitions :: Connection -> IO()
showAllCompetitions conn = do
  result <- getAllCompetitions conn
  if null(result) == True
    then putStrLn("No competition found")
  else mapM_ print result

showAllCompetitionsForSection :: Connection -> IO()
showAllCompetitionsForSection conn = do
  putStr("Section ID: ")
  hFlush stdout
  sectionIdStr <- getLine
  let sectionId = (read sectionIdStr :: Int64)

  section <- getSection conn sectionId
  if null(section) == True
    then putStrLn("Section with this ID does not exist")
  else do
    result <- getAllCompetitionsForSection conn sectionId
    if null(result) == True
      then putStrLn("No competition found")
    else mapM_ print result



showCreateCompetition :: Connection -> IO()
showCreateCompetition conn = do
  putStr("Section ID: ")
  hFlush stdout
  sectionIdStr <- getLine
  let sectionId = (read sectionIdStr :: Int64)

  section <- getSection conn sectionId
  if null(section) == True
    then putStrLn("Section with this ID does not exist")
  else do
    putStr("Start timestamp (yyyy-mm-dd hh:mm:ss): ")
    hFlush stdout
    startTimestampStr <- getLine
    let startTimestamp = parseTimestamp startTimestampStr

    putStr("Venue: ")
    hFlush stdout
    venue <- getLine

    createCompetition conn sectionId startTimestamp venue
    putStr("")


showEditCompetition :: Connection -> IO()
showEditCompetition conn = do
  putStr("Enter Competition ID: ")
  hFlush stdout

  idStr <- getLine
  let id = (read idStr :: Int64)

  competition <- getCompetition conn id
  if null(competition) == True
    then putStrLn("Competition with such ID does not exist")
  else do
    mapM_ print competition

    putStrLn("")
    putStrLn("What do you want to edit?")
    putStrLn("  1) start timestamp")
    putStrLn("  2) venue")
    putStrLn("  3) [back]")
    putStr("Option: ")
    hFlush stdout

    resp <- getChar
    _ <- getLine

    case resp of
      '1' -> do
        putStr("Start timestamp (yyyy-mm-dd hh:mm:ss): ")
        hFlush stdout
        startTimestampStr <- getLine
        let startTimestamp = parseTimestamp startTimestampStr

        res <- updateCompetitionStartTimestamp conn id startTimestamp
        if res == True
          then putStrLn("Updated successfully")
        else putStrLn("Not updated")


      '2' -> do
        putStr("Venue: ")
        hFlush stdout
        venue <- getLine

        res <- updateCompetitionVenue conn id venue
        if res == True
          then putStrLn("Updated successfully")
        else putStrLn("Not updated")


      _ -> putStrLn("")



showDeleteCompetition :: Connection -> IO()
showDeleteCompetition conn = do
  putStr("Enter Competition ID: ")
  hFlush stdout

  idStr <- getLine
  let id = (read idStr :: Int64)

  competition <- getCompetition conn id
  if null(competition) == True
    then putStrLn("Competition with such ID does not exist")
  else do
    mapM_ print competition

    putStr("Do you want to delete? (y/n) ")
    hFlush stdout

    resp <- getChar
    _ <- getLine

    if resp == 'y'
      then do
        res <- deleteCompetition conn id
        if res == True
          then putStrLn("Deleted successfully!")
        else putStrLn("Not deleted")
    else putStrLn("")
