module Main (main) where

import Lib
import Data.Array
import Control.Monad

printArray :: Array (Int, Int) Int -> String
printArray arr = unlines [unwords [show (arr ! (x, y)) | x <- [0..7]] | y <- [0..7]]

print2DArray :: Array (Int, Int) Int -> IO ()
print2DArray arr = do
    let ((minX, minY), (maxX, maxY)) = bounds arr
    forM_ [minX .. maxX] $ \i -> do
        forM_ [minY .. maxY] $ \j -> do
            putStr $ show (arr ! (i, j)) ++ " "
        putStrLn "" -- Move to the next line after printing a row

main :: IO ()
main = do
  --putStrLn(show $ isCellValid (-1) 1 2)
--  putStrLn(getPossibleMovesSimple (1,1))
  let board = array ((0,0), (7,7)) [((i,j), 0) | i <-[0..7], j <- [0..7]]
  let cell = (5, 5)
  let degree = getCellDegree cell board
  let minDegree = getCellSecondMinDegree (0,0) (board // [((i,j), 1) | i<-[1], j<-[1]])

  let paths = array ((0,0), (7,7)) [((i,j), (length(findPath (i,j)))) | i <- [0..7], j <- [0..7]]

--  putStrLn(show $ paths!(0,0))
  print2DArray paths

--  putStrLn([show (paths!(x, y)) | x <- [0..7], y <- [0..7]])
--  let path = findPath (0,1)
{-
  mapM_ print (getPossibleDestinations cell board)


  putStr("Degree: ")
  putStrLn(show degree)

  putStr("Min degree: ")
  putStrLn(show minDegree)
-}

{-
  putStrLn("Path: ")
  mapM_ print (path)

  putStr("Path length: ")
  putStrLn(show $ length(path))
-}

