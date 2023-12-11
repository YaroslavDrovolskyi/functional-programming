module Menus.MainMenu where

import Database.PostgreSQL.Simple
import Menus.StudentMenu
import Menus.InstructorMenu
import System.IO

mainMenu :: Connection -> IO()
mainMenu conn = do
  putStrLn("")
  putStrLn("======================= Main menu =======================")
  putStrLn("  1) Student menu")
  putStrLn("  2) Instructor menu")
  putStrLn("  3) Sections menu")
  putStrLn("  4) Competitions menu")
  putStrLn("  5) SectionStudents menu")
  putStrLn("  6) SectionInstructors menu")
  putStrLn("  7) SectionLessons menu")
  putStrLn("  e) Exit")
  putStrLn("=========================================================")
  putStr("Option: ")
  hFlush stdout --- flush stdout in order to putStr be printed, https://www.reddit.com/r/haskellquestions/comments/lx5jgb/force_putstr/

  resp <- getChar -- get entered char
  _ <- getLine -- flush entire row; because without it, in case when user enters line, characters from line will be read in next getChar


  case resp of
    '1' -> studentMainMenu conn
    '2' -> instructorMainMenu conn
    '3' -> putStrLn("3 selected")
    '4' -> putStrLn("4 selected")
    '5' -> putStrLn("4 selected")
    '6' -> putStrLn("4 selected")
    '7' -> putStrLn("4 selected")
    _ -> putStr("")


-- make main menu in loop until 'e' is pressed
  if resp /= 'e'
   then mainMenu conn
  else putStr("")
