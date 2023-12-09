{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import Database.PostgreSQL.Simple
import Entities.Student



retrieveBook :: Connection -> Int -> IO[(Int, String, Int, Int, Int)]
retrieveBook conn id = query conn "SELECT id, title, number_of_pages, publishing_year, quantity FROM books where id = ?" $ (Only id)


retrieveStudent :: Connection -> Int -> IO[Student]
retrieveStudent conn id = query conn "SELECT * FROM students where id = ?" $ (Only id)

main :: IO ()
main = do
  conn <- connectPostgreSQL "host='localhost' port=5432 dbname='faculty_sport' user='postgres' password='cnjrjl56cfgj;rb99'"
--  mapM_ print =<< (query_ conn "SELECT book_id FROM book_requests WHERE id=304" :: IO [Only Int])
--  mapM_ print =<< retrieveBook conn 104
  mapM_ print =<< retrieveStudent conn 1
--  query_ conn [sql| select 2+2 |] :: IO [Only Int]
