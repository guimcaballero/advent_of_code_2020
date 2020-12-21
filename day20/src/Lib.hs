{-# LANGUAGE TupleSections #-}

module Lib
    ( someFunc
    ) where

import Prelude hiding (Either, Left, Right)

import Data.Maybe
import Data.Function
import Data.List
import Data.List.Extra (groupOn)
import Data.Tuple.Extra (fst3, thd3)

import Data.Map (Map)
import qualified Data.Map as Map

someFunc :: IO ()
someFunc = part2

-------------------
-- Parsing
-------------------

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn p s = cons (case break p s of
                                    (l, s') -> (l, case s' of
                                                    []      -> []
                                                    _:s''   -> splitOn p s''))
  where
    cons ~(h, t)        =  h : t

parseTile :: [String] -> Tile
parseTile (num:strings) = (parseNum $ words num, parseSides strings, newStrings)
  where
    parseNum ["Tile",n] = read $ head $ splitOn (== ':') n
    parseNum _ = error "Parse error"

    parseSides list = (top, right, bottom, left)
      where top = head list
            right = map last list
            bottom = last list
            left = map head list


    -- str = map (\s -> ' ':s ++ [' ']) strings
    -- smth = replicate (length $ head str) ' '
    -- newStrings = smth:str ++ [smth]
    newStrings = map (init . tail) $ init $ tail strings

parseTile _ = error "Parse error"


-------------------
-- Transformations
-------------------

flipVertical, flipHorizontal, rotateRight1, rotateRight2, rotateRight3 :: Tile -> Tile
flipVertical (num, (top, right, bottom, left), strings) = (num, (bottom, reverse right, top, reverse left), reverse strings)
flipHorizontal (num, (top, right, bottom, left), strings) = (num, (reverse top, left, reverse bottom, right), map reverse strings)
rotateRight1 (num, (top, right, bottom, left), strings) = (num, (reverse left, top, reverse right, bottom), map reverse $ transpose strings)
rotateRight2 = rotateRight1 . rotateRight1
rotateRight3 = rotateRight1 . rotateRight1 . rotateRight1

tileAlternatives :: Tile -> [Tile]
tileAlternatives tile =
  nub $ map
    (\s -> s tile)
    [ id
    , flipVertical
    , flipHorizontal
    , rotateRight1
    , rotateRight2
    , rotateRight3
    , rotateRight1 . flipVertical
    , rotateRight1 . flipHorizontal
    , rotateRight2 . flipVertical
    , rotateRight2 . flipHorizontal
    , rotateRight3 . flipVertical
    , rotateRight3 . flipHorizontal
    , flipVertical  . rotateRight1
    , flipHorizontal . rotateRight1
    , flipVertical  . rotateRight2
    , flipHorizontal . rotateRight2
    , flipVertical  . rotateRight3
    , flipHorizontal . rotateRight3
    , flipHorizontal . flipVertical
    , flipVertical . flipHorizontal
    ]

-------------------
-- Main logic
-------------------

type Tile = (Int, Sides, [String])
           -- Top,    Right,  Bottom, Left
type Sides = (String, String, String, String)
data Side = Top | Right | Bottom | Left
  deriving (Show, Eq)
type Position = (Int, Int)
type Tracker = Map Position Tile

neighbors :: Position -> [(Side, Position)]
neighbors (x, y) = [(Right, (x + 1, y)), (Left, (x - 1, y)), (Top, (x, y + 1)), (Bottom, (x, y - 1))]

neighborsWithNoTile :: Tracker -> [Position]
neighborsWithNoTile tracker = nub (map snd $ concatMap neighbors $ Map.keys tracker) \\ Map.keys tracker

-- Returns with which side the first tile matches the second
tilesMatch :: Tile -> Tile -> Maybe Side
tilesMatch (_, (top1, right1, bottom1, left1), _) (_, (top2, right2, bottom2, left2), _)
  | top1 == bottom2 = Just Top
  | bottom1 == top2 = Just Bottom
  | right1 == left2 = Just Right
  | left1 == right2 = Just Left
  | otherwise = Nothing

solveTracker :: [Tile] -> Tracker -> [Tracker]
solveTracker [] tracker = [tracker]
solveTracker tiles tracker
  -- | Map.size tracker == 8 = [tracker]
  | Map.size tracker == 0 = solveTracker (tail tiles) $ Map.singleton (0,0) $ head tiles
  | otherwise = nub $ concatMap (updateAndSolve tracker tiles) filledTiles
  where
    len = length tiles + Map.size tracker
    boundCheck = inBounds tracker len

    getTiles pos = map (pos, ) $ checkListOfTilesForPosition tracker pos tiles
    filledTiles = concatMap getTiles $ filter boundCheck $ neighborsWithNoTile tracker

updateAndSolve :: Tracker -> [Tile] -> (Position, Tile) -> [Tracker]
updateAndSolve tracker tiles = uncurry solveTracker . updateTrackerAndTileListWithTile tracker tiles

inBounds :: Tracker -> Int -> (Int, Int) -> Bool
inBounds tracker len (a, b) = and $ map help $ Map.keys tracker
  where
    square = (floor . sqrt . fromIntegral) len
    help (x, y) = abs (x - a) < square && abs (y - b) < square

updateTrackerAndTileListWithTile :: Tracker -> [Tile] -> (Position, Tile) -> ([Tile], Tracker)
updateTrackerAndTileListWithTile tracker tiles (pos, tile) = (newTiles, Map.insert pos tile tracker)
  where newTiles = filter (\(num, _, _) -> num /= fst3 tile) tiles

-- Checks if any of the tiles fits in position
checkListOfTilesForPosition :: Tracker -> Position -> [Tile] -> [Tile]
checkListOfTilesForPosition tracker pos tiles =
  [ t
  | tile <- tiles
  , t <- tileAlternatives tile
  , checkTileForPosition tracker pos t
  ]

-- Returns True if the tile can fit in the position
checkTileForPosition :: Tracker -> Position -> Tile -> Bool
checkTileForPosition tracker (x, y) tile = and nearbies
    -- Get all the tiles nearby that exist
  where
    nearbies =
      [ Just side == tilesMatch tile t
      | (side, pos) <- neighbors (x, y)
      , Just t <- [Map.lookup pos tracker]
      ]

multCorners :: Tracker -> Int
multCorners tracker = product $ [fst3 (tracker Map.! pos) | pos <- [(maxX, maxY), (minX, minY), (maxX, minY), (minX, maxY)]]
  where keys = Map.keys tracker
        xs = map fst keys
        ys = map snd keys
        maxX = maximum xs
        maxY = maximum ys
        minX = minimum xs
        minY = minimum ys

part1 :: IO ()
part1 = do
  tiles <- map parseTile . splitOn (== "") . lines <$> readFile "input.txt"

  let solved = head $ solveTracker tiles Map.empty
  mapM_ print $ joinTrackerIntoGrid $ Map.map (\s -> singleton $ (++ " ") $ show $ fst3 s) solved
  print $ multCorners solved

joinTrackerIntoGrid :: Map Position [String] -> [[Char]]
joinTrackerIntoGrid tracker = concat $ reverse $ map (foldl1' (zipWith (++))) $ map (map snd) $ groupOn (snd . fst) $ sortOn (snd . fst) $ Map.assocs tracker

singleton a = [a]

type Sea = Map Position Char

imageToSea :: [String] -> Sea
imageToSea image =
  Map.fromList
    [ ((x, y), char)
    | (y, string) <- zip [0 ..] image
    , (x, char) <- zip [0 ..] string
    ]

seaToImage :: Sea -> [String]
seaToImage image = map (map snd) $ groupOn (snd . fst) $ sortOn (snd . fst) $ Map.assocs image

findMonsters :: Sea -> Sea
findMonsters sea = Map.union newSea sea
  where
    monsterPositions = filter (isMonster sea) $ Map.keys sea
    os = concat $ map monsterPossibilities monsterPositions
    newSea = Map.fromList [ (pos, 'O') | pos <- os]

monsterPossibilities (x, y) =
  [ (x + 18, y)
  , (x + 0,  y + 1)
  , (x + 5,  y + 1)
  , (x + 6,  y + 1)
  , (x + 11, y + 1)
  , (x + 12, y + 1)
  , (x + 17, y + 1)
  , (x + 18, y + 1)
  , (x + 19, y + 1)
  , (x + 1,  y + 2)
  , (x + 4,  y + 2)
  , (x + 7,  y + 2)
  , (x + 10, y + 2)
  , (x + 13, y + 2)
  , (x + 16, y + 2)
  ]

isMonster :: Sea -> Position -> Bool
isMonster sea position = 15 == (length $ filter (/= Nothing) [ check pos | pos <- monsterPossibilities position ])
  where
    check pos = do
      char <- Map.lookup pos sea
      if char == '#' then Just True else Nothing

countHash :: Sea -> Int
countHash sea = length $ filter (=='#') $ Map.elems sea

imageAlternatives :: [String] -> [[String]]
imageAlternatives image = map (image &) [ reverse, map reverse, rotate, rotate . rotate, rotate . rotate . rotate, rotate . reverse, rotate . (map reverse) ]
  where
    rotate = (map reverse) . transpose

roughWater image = countHash monsterSea
  where
    sea = imageToSea image
    monsterSea = findMonsters sea


part2 :: IO ()
part2 = do
  raw <- splitOn (== "") . lines <$> readFile "input.txt"
  let tiles = map parseTile raw

  let final = head $ solveTracker tiles Map.empty
  let solved = Map.map thd3 final

  let image = joinTrackerIntoGrid solved

  print $ minimum $ map roughWater $ imageAlternatives image
