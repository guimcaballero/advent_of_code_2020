module Lib
    ( someFunc
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

someFunc :: IO ()
someFunc = part2

data PositionState = Floor | Empty | Occupied
  deriving (Show, Eq)
type Positions = Map Position PositionState --[[PositionState]]
type Position = (Int, Int)

charToPosition :: Char -> PositionState
charToPosition char = case char of
                        '.' -> Floor
                        'L' -> Empty
                        '#' -> Occupied
positionToChar :: Maybe PositionState -> Char
positionToChar pos =
  case pos of
    Nothing -> ' '
    Just char ->
      case char of
        Floor -> '.'
        Empty -> 'L'
        Occupied -> '#'

countOccupied :: [Maybe PositionState] -> Int
countOccupied list = sum $ map smth list
  where
    smth pos = case pos of
                 Nothing -> 0
                 Just state -> case state of
                                Floor -> 0
                                Empty -> 0
                                Occupied -> 1

type UpdatePosition = Positions -> Position -> PositionState -> PositionState

updatePosition1 :: Positions -> Position -> PositionState -> PositionState
updatePosition1 positions (x, y) prev =
  case prev of
    Floor -> Floor
    Empty ->
      if occ == 0
        then Occupied
        else Empty
    Occupied ->
      if occ >= 4
        then Empty
        else Occupied
    -- a    b   c
    -- d  prev  e
    -- f    g   h
  where
    a = Map.lookup (x - 1, y - 1) positions
    b = Map.lookup (x, y - 1) positions
    c = Map.lookup (x + 1, y - 1) positions
    d = Map.lookup (x - 1, y) positions
    e = Map.lookup (x + 1, y) positions
    f = Map.lookup (x - 1, y + 1) positions
    g = Map.lookup (x, y + 1) positions
    h = Map.lookup (x + 1, y + 1) positions
    occ = countOccupied [a, b, c, d, e, f, g, h]


stepSimulation :: (UpdatePosition) -> Positions -> Positions
stepSimulation updatePosition prevPositions = newPositions
  where
     fun = updatePosition prevPositions
     newPositions = Map.mapWithKey fun prevPositions

stepUntilStable :: (UpdatePosition) -> Positions -> Positions
stepUntilStable updatePosition positions =
  if positions == newPositions
    then positions
    else stepUntilStable updatePosition newPositions
  where
    newPositions = stepSimulation updatePosition positions

countOccupiedInMap :: Positions -> Int
countOccupiedInMap positions = length $ filter (== Occupied) $ Map.elems positions

part1 :: IO ()
part1 = do
  raw <- lines <$> readFile "input.txt"
  let positionsList = map (map charToPosition) raw
  let width = length $ head positionsList
  let height = length positionsList

  let positions =
        Map.fromList
          [ ((x, y), (positionsList !! y) !! x)
          | x <- [0 .. width - 1]
          , y <- [0 .. height - 1]
          ] :: Positions

  let end = stepUntilStable updatePosition1 positions
  print $ countOccupiedInMap end

  -- let endString = [ [ positionToChar (Map.lookup (x, y) end) |  x <- [0..width - 1] ] | y <- [0..height - 1]  ]
  -- mapM_ print endString


firstSeenSeat :: Positions -> (Int, Int) -> Position -> Maybe PositionState
firstSeenSeat positions (x, y) (dx, dy) = do
  let newPos = (x + dx, y + dy)
  seat <- Map.lookup newPos positions
  if seat == Floor then firstSeenSeat positions newPos (dx, dy) else Just seat

updatePosition2 :: Positions -> Position -> PositionState -> PositionState
updatePosition2 positions (x, y) prev =
  case prev of
    Floor -> Floor
    Empty ->
      if occ == 0
        then Occupied
        else Empty
    Occupied ->
      if occ >= 5
        then Empty
        else Occupied
    -- a    b   c
    -- d  prev  e
    -- f    g   h
  where
    fun = firstSeenSeat positions (x, y)
    a = fun (-1, -1) --Map.lookup (x - 1, y - 1) positions
    b = fun (0, -1) --Map.lookup (x, y - 1) positions
    c = fun (1, -1) --Map.lookup (x + 1, y - 1) positions
    d = fun (-1, 0) --Map.lookup (x - 1, y) positions
    e = fun (1, 0) --Map.lookup (x + 1, y) positions
    f = fun (-1, 1) --Map.lookup (x - 1, y + 1) positions
    g = fun (0, 1) --Map.lookup (x, y + 1) positions
    h = fun (1, 1) --Map.lookup (x + 1, y + 1) positions
    occ = countOccupied [a, b, c, d, e, f, g, h]


part2 :: IO ()
part2 = do
  raw <- lines <$> readFile "input.txt"
  let positionsList = map (map charToPosition) raw
  let width = length $ head positionsList
  let height = length positionsList

  let positions =
        Map.fromList
          [ ((x, y), (positionsList !! y) !! x)
          | x <- [0 .. width - 1]
          , y <- [0 .. height - 1]
          ] :: Positions

  let end = stepUntilStable updatePosition2 positions
  print $ countOccupiedInMap end
