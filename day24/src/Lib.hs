module Lib
    ( someFunc
    ) where

import Data.List
import Data.Functor (($>))
import Text.ParserCombinators.Parsec

import Data.Map (Map)
import qualified Data.Map as Map


someFunc :: IO ()
someFunc = do
  directions <- map (fromRight . parse parseDirections "directions") . lines <$> readFile "input.txt"

  let tracker = part1 directions
  print $ Map.size tracker
  print $ part2 tracker

-------------------
-- Parsing
-------------------

parseDirections :: Parser [Direction]
parseDirections = do
  dirs <- many1 parseDirection
  return dirs

parseDirection :: Parser Direction
parseDirection = do
      try (string "se") $> SE
    <|> try (string "sw") $> SW
    <|> try (string "nw") $> NW
    <|> try (string "ne") $> NE
    <|> try (string "e") $> E
    <|> try (string "w") $> W

fromRight (Right x) = x
fromRight _ = error "Unwrapping Left"

-------------------
-- Main logic
-------------------

data Direction = E| SE | SW | W | NW | NE
  deriving (Show, Eq)
data Color = Black | White
  deriving (Show, Eq)
type Position = (Int, Int, Int)
type Tracker = Map Position Color

moveInDirection ::  Direction -> Position -> Position
moveInDirection E  (x, y, z) = (x + 1, y - 1, z)
moveInDirection SE (x, y, z) = (x,     y - 1, z + 1)
moveInDirection SW (x, y, z) = (x - 1, y,     z + 1)
moveInDirection W  (x, y, z) = (x - 1, y + 1, z)
moveInDirection NW (x, y, z) = (x,     y + 1, z - 1)
moveInDirection NE (x, y, z) = (x + 1, y,     z - 1)

coordFromDirections :: [Direction] -> Position
coordFromDirections dirs = foldr moveInDirection (0, 0, 0) dirs

flipTiles :: [Position] -> Tracker -> Tracker
flipTiles [] tracker = tracker
flipTiles (pos:rest) tracker = flipTiles rest $ Map.alter help pos tracker
  where
    help Nothing = Just Black
    help (Just _) = Nothing

part1 :: [[Direction]] -> Tracker
part1 dirs = flipTiles coords Map.empty
  where
    coords = map (coordFromDirections) dirs

-- Counts the number of Maybe states that are Active
countBlack :: [Maybe Color] -> Int
countBlack list = length $ filter (== Just Black) list

-- Counts the number of neighbors that are Active
countBlackNeighbors :: Tracker -> Position -> Int
countBlackNeighbors tracker coord = countBlack $ map (flip Map.lookup tracker) $ neighbors coord

neighbors :: Position -> [Position]
neighbors pos = map (flip moveInDirection pos) [E, SE, SW, W, NW, NE]

updateCoord :: Tracker -> Position -> Color -> Color
updateCoord tracker coord prev =
  case prev of
    Black ->
      if activeNeighbors == 0 || activeNeighbors > 2
        then White
        else Black
    White ->
      if activeNeighbors == 2
        then Black
        else White
  where
    activeNeighbors = countBlackNeighbors tracker coord

advanceDay :: Tracker -> Tracker
advanceDay tracker = newTracker
    where
     -- Get keys around
     keys = nub $ concat $ map neighbors $ Map.keys tracker
     -- Add the ones on keys that aren't on pocket as inactive
     surround = foldr (Map.alter help) tracker keys
     help Nothing = Just White
     help a = a

     newTracker = Map.mapWithKey (updateCoord surround) surround

advanceNDays 0 tracker = tracker
advanceNDays n tracker = advanceNDays (n - 1) $ advanceDay tracker

part2 :: Tracker -> Int
part2 tracker = length $ filter (== Black) $ Map.elems $ advanceNDays 100 tracker
