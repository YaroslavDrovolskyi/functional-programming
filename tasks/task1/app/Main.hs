module Main (main) where

import Lib
import Data.Array
import Control.Monad

printArray :: Array (Int, Int) Int -> String
printArray arr = unlines [unwords [show (arr ! (x, y)) | x <- [0..7]] | y <- [0..7]]

getNumberWidth :: Int -> Int
getNumberWidth n = length(show n)

print2DArray :: Array (Int, Int) Int -> IO ()
print2DArray arr = do
    let ((minX, minY), (maxX, maxY)) = bounds arr
    forM_ [minX .. maxX] $ \i -> do
        forM_ [minY .. maxY] $ \j -> do
            let elemWidth = getNumberWidth(arr ! (i, j))
            putStr $ show (arr ! (i, j)) ++ (replicate (3 - elemWidth) ' ')
        putStrLn ""

first :: ([(Int, Int)], Array (Int, Int) Int) -> [(Int, Int)]
first (a, b) = a

main :: IO ()
main = do
  let (path, board) = findPath (6, 7)


--------- test to check if path from each is being calculated
--  let pathsLengths = array ((0,0), (7,7)) [((i,j), (length(first(findPath (i,j))))) | i <- [0..7], j <- [0..7]]
--  print2DArray pathsLengths




  putStrLn("Path: ")
  mapM_ print (path)

  putStrLn("")
  putStr("Path length: ")
  putStrLn(show $ length(path))


  putStrLn("")
  putStrLn("")
  putStrLn("Board:")
  print2DArray board



--  putStrLn(printArray paths)

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

-}

