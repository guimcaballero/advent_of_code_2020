{-# LANGUAGE Strict, TupleSections, StrictData #-}

module Lib
    ( someFunc
    ) where

import           Data.List

import           Data.Map (Map)
import qualified Data.Map as Map

someFunc :: IO ()
someFunc = part1

-----------------
-- Definitions --
-----------------

data State = Active | Inactive
  deriving (Show, Eq)

-- (x, y , z) | (x, y, z, w)
data Coord = Dim3 (Int, Int, Int) | Dim4 (Int, Int, Int, Int)
  deriving (Show, Eq, Ord)
type Pocket = Map Coord State

-----------------
-- Main logic  --
-----------------

charToState :: Char -> State
charToState '.' = Inactive
charToState '#' = Active
charToState a = error $ "Parsing unexpected Char: " ++ [a]

-- Counts the number of Maybe states that are Active
countActive :: [Maybe State] -> Int
countActive list = length $ filter (== Just Active) list

-- Counts the number of neighbors that are Active
countActiveNeighbors :: Pocket -> Coord -> Int
countActiveNeighbors pocket coord = countActive $ map (flip Map.lookup pocket) $ neighbors coord

-- Generates a list of all the neighboring coordinates
neighbors :: Coord -> [Coord]
neighbors (Dim3 (x, y, z)) = [ Dim3 (x + a, y + b, z + c) | a <- [-1, 0, 1], b <- [-1, 0, 1], c <- [-1, 0, 1], not (a == 0 && b == 0 && c == 0) ]
neighbors (Dim4 (x, y, z, w)) = [ Dim4 (x + a, y + b, z + c, w + d) | a <- [-1, 0, 1], b <- [-1, 0, 1], c <- [-1, 0, 1], d <- [-1, 0, 1], not (a == 0 && b == 0 && c == 0 && d == 0) ]

-- Gets the new State according to a pocket and a coordinate
updateCoord :: Pocket -> Coord -> State -> State
updateCoord pocket coord prev =
  case prev of
    Active ->
      if activeNeighbors == 2 || activeNeighbors == 3
        then Active
        else Inactive
    Inactive ->
      if activeNeighbors == 3
        then Active
        else Inactive
  where
    activeNeighbors = countActiveNeighbors pocket coord

-- Steps the simulation once
stepSimulation :: Pocket -> Pocket
stepSimulation prevPocket = newPocket
  where
     -- Get keys around
     keys = nub $ concat $ map neighbors $ Map.keys prevPocket
     -- Add the ones on keys that aren't on pocket as inactive
     surround = foldr (Map.alter help) prevPocket keys
     help Nothing = Just Inactive
     help a = a

     newPocket = Map.mapWithKey (updateCoord surround) surround

-- Does times iterations of the simulation
stepTimes :: Int -> Pocket -> Pocket
stepTimes times pocket = stepHelper times 0 pocket
  where
    stepHelper times iter pocket
      | times == iter = pocket
      | otherwise = stepHelper times (iter + 1) (stepSimulation pocket)

part1 :: IO ()
part1 = do
  states <- map (map charToState) . lines <$> readFile "input.txt"

  let width = length $ head states
  let height = length states
  let pocket =
        Map.fromList
          [ (Dim3 (x, y, 0), (states !! y) !! x)
          | x <- [0 .. width - 1]
          , y <- [0 .. height - 1]
          ] :: Pocket

  let final = stepTimes 6 pocket
  print $ length $ filter (== Active) $ Map.elems final

-- Runs very slowly, but it works
part2 :: IO ()
part2 = do
  states <- map (map charToState) . lines <$> readFile "input.txt"

  let width = length $ head states
  let height = length states
  let pocket =
        Map.fromList
          [ (Dim4 (x, y, 0, 0), (states !! y) !! x)
          | x <- [0 .. width - 1]
          , y <- [0 .. height - 1]
          ] :: Pocket

  let final = stepTimes 6 pocket
  print $ length $ filter (== Active) $ Map.elems final
