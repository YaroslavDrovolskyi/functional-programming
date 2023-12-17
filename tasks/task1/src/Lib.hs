module Lib where

import Data.Array

-- check if cell is valid in terms of n x n chess board
-- x, y is coordinates of cell
-- n is size of board
isCellValid :: (Int, Int) -> Int -> Bool
isCellValid (x, y) n =
  (x >= 0) && (y >= 0) && (x < n) && (y < n)

getRawPossibleDestinations :: (Int, Int) -> [(Int, Int)]
getRawPossibleDestinations (x,y) =
  [(x+1,y+2), (x+2,y+1), (x+1,y-2), (x-1,y+2), (x-1,y-2), (x+2,y-1), (x-2,y+1), (x-2,y-1)]

-- board is two-dimensional array; board[i][j] is 0 if cell (i,j) is not visited by horse
isValidDestination :: (Int, Int) -> Array (Int, Int) Int -> Bool
isValidDestination (x,y) board = (isCellValid (x,y) 8) && ((board!(x,y)) == 0)

-- returns list of possible destinations for Horse from given cell (x,y)
-- board is two-dimensional array denotes chess board
getPossibleDestinations :: (Int, Int) -> Array (Int, Int) Int -> [(Int, Int)]
getPossibleDestinations (x, y) board =
  filter (\(a,b) -> isValidDestination (a,b) board) (getRawPossibleDestinations (x,y))

-- returns count of possible destinations for Horse from given cell (x,y)
-- board is two-dimensional array denotes chess board
getCellDegree :: (Int, Int) -> Array (Int, Int) Int -> Int
getCellDegree (x,y) board =
  length(getPossibleDestinations (x,y) board)

-- returns the min degree of cells, that can be accessed from given cell (x,y)
getCellSecondMinDegree :: (Int, Int) -> Array (Int, Int) Int -> Int
getCellSecondMinDegree (x,y) board =
  let destinations = getPossibleDestinations (x,y) board
      destinationsDegrees = map (\(i,j) -> getCellDegree (i,j) (board // [((m,n),-1) | m <- [x], n <- [y]])) (destinations)

  in minimum destinationsDegrees

-- recursive function of finding path
-- path is list of cells
-- cell (x,y) is already ticked on board
findPathImpl :: (Int, Int) -> Array (Int, Int) Int -> Int -> [(Int, Int)] -> ([(Int, Int)], Array (Int, Int) Int)
findPathImpl (x,y) board boardSize path =
  if length(path) >= boardSize * boardSize
    then (path, board)
  else
    let destinations = getPossibleDestinations (x,y) board
        destinationsDegrees = map (\(i,j) -> getCellDegree (i,j) board) (destinations)
        minDestinationDegree = minimum destinationsDegrees
        minDestinations = filter (\(i,j) -> (getCellDegree (i,j) board) == minDestinationDegree) (destinations) -- destinations with min degrees

        (destX, destY) = head minDestinations
        
    in
      if length(minDestinations) > 1 -- tie is risen
        then
          let secondDestinationsDegrees = map (\(i,j) -> getCellSecondMinDegree (i,j) board) (minDestinations)
              minDestinationSecondDegree = minimum secondDestinationsDegrees
              minSecondDestinations = filter (\(i,j) -> (getCellSecondMinDegree (i,j) board) == minDestinationSecondDegree) (minDestinations) -- destinations with min second degrees
              (destX, destY) = head minSecondDestinations
          in
            findPathImpl (destX, destY) (board // [((i,j), length(path) + 1) | i <- [destX], j <- [destY]]) boardSize (path ++ [(destX, destY)])

      else
        findPathImpl (destX, destY) (board // [((i,j), length(path) + 1) | i <- [destX], j <- [destY]]) boardSize (path ++ [(destX, destY)])


-- returns (<path (list of visited sells)>, <chessboard, with number in each cell means order of that cell in path>)
findPath :: (Int, Int) -> ([(Int, Int)], Array (Int, Int) Int)
findPath (x,y) =
  let boardEmpty = array ((0,0), (7,7)) [((i,j), 0) | i <- [0..7], j <- [0..7]]
      boardInit = boardEmpty // [((i,j), 1) | i <- [x], j <- [y]]

  in
      findPathImpl (x,y) boardInit 8 [(x,y)]

-- 	board[x_pos][y_pos] = moves_count;

-- Source: https://medium.com/@danielfrost_3076/implementing-a-heuristic-solution-to-the-knights-tour-problem-513a73cc7e20

