module Lib
    ( someFunc
    ) where

import Prelude hiding (Left, Right)

someFunc :: IO ()
someFunc = part2

data CardinalDirection = North | East | South | West
  deriving (Show, Eq)
data RelativeDirection = Forward | Right | Left
  deriving (Show, Eq)
type Ship = (CardinalDirection, (Int, Int))

data Action = C CardinalDirection | R RelativeDirection
  deriving (Show, Eq)
type Instruction = (Action, Int)

parseInstructions :: [String] -> [Instruction]
parseInstructions list = map parse list
  where
    parse line = (dir, read $ tail line)
      where
        dir =
          case head line of
            'N' -> C North
            'S' -> C South
            'E' -> C East
            'W' -> C West
            'L' -> R Left
            'R' -> R Right
            'F' -> R Forward
            _ -> error "Unexpected character"

turnDirection original turn val
  | val == 0 = original
  | turn == Right = case original of
                     North -> turnDirection East turn (val - 90)
                     East -> turnDirection South turn (val - 90)
                     South -> turnDirection West turn (val - 90)
                     West -> turnDirection North turn (val - 90)
  | turn == Left = case original of
                     North -> turnDirection West turn (val - 90)
                     West -> turnDirection South turn (val - 90)
                     South -> turnDirection East turn (val - 90)
                     East -> turnDirection North turn (val - 90)
  | otherwise = error "Can't turn forward"

moveForward :: Ship -> Int -> Ship
moveForward (dir, (x, y)) val = case dir of
                              North -> (dir, (x, y + val))
                              South -> (dir, (x, y - val))
                              East -> (dir, (x + val, y))
                              West -> (dir, (x - val, y))

moveShip :: Ship -> Instruction -> Ship
moveShip (dir, (x, y)) (action, val) = case action of
                              C North -> (dir, (x, y + val))
                              C South -> (dir, (x, y - val))
                              C East -> (dir, (x + val, y))
                              C West -> (dir, (x - val, y))
                              R Forward -> moveForward (dir, (x, y)) val
                              R a -> (turnDirection dir a val, (x, y))

part1 :: IO ()
part1 = do
  raw <- lines <$> readFile "input.txt"
  let instructions = parseInstructions raw

  let newShip = (East, (0,0))

  let final = foldl moveShip newShip instructions

  let (_, (x, y)) = final
  print $ abs x + abs y



type ShipWaypoint = ((Int, Int), (Int, Int))

turnWaypoint :: RelativeDirection -> (Int, Int) -> (Int, Int) -> Int -> (Int, Int)
turnWaypoint turn (x, y) (wx, wy) val
  | val == 0 = (wx, wy)
  | val == 180 = (-wx, -wy)
  | turn == Left =
    case val of
      90 -> (-wy, wx)
      270 -> (wy, -wx)
  | turn == Right =
    case val of
      270 -> (-wy, wx)
      90 -> (wy, -wx)
  | otherwise = error "Can't move waypoint forward"

moveShipOrWaypoint :: ShipWaypoint -> Instruction -> ShipWaypoint
moveShipOrWaypoint  ((x, y), (wx, wy)) (action, val) = case action of
                              C North -> ((x, y), (wx, wy + val))
                              C South -> ((x, y), (wx, wy - val))
                              C East -> ((x, y), (wx + val, wy))
                              C West -> ((x, y), (wx - val, wy))
                              R Forward -> ((x + val * wx, y + val * wy), (wx, wy))
                              R a -> ((x, y), turnWaypoint a (x,y) (wx, wy) val)

part2 :: IO ()
part2 = do
  raw <- lines <$> readFile "input.txt"
  let instructions = parseInstructions raw

  let newShip = ((0, 0), (10, 1))

  let final = foldl moveShipOrWaypoint newShip instructions
  print final
  let ((x, y), _) = final
  print (abs x + abs y)
