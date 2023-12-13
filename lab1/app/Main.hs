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



main :: IO ()
main = do
  conn <- connectPostgreSQL "host='localhost' port=5432 dbname='faculty_sport' user='postgres' password='cnjrjl56cfgj;rb99'"

  mainMenu conn