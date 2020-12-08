module Lib
    ( someFunc
    ) where

import Data.List


someFunc :: IO ()
someFunc = part2


data BTree = Leaf Int | Branch BTree BTree
  deriving (Eq,Show)

makeTree :: [Int] -> BTree
makeTree [num] = Leaf num
makeTree nums = Branch (makeTree first) (makeTree second)
  where
    half = length nums `div` 2
    (first, second) = splitAt half nums


traverseTree :: BTree -> [Side] -> Int
traverseTree (Leaf i) [] = i
traverseTree (Branch a b) (x:xs) = traverseTree branch xs
  where
    branch = case x of
               Lower -> a
               Upper -> b
traverseTree tree sides = error $ "Oops! " ++ show tree ++ show sides

data Side = Lower | Upper
  deriving (Show)

fbrlToSide :: Char -> Side
fbrlToSide char = case char of
                'F' -> Lower
                'L' -> Lower
                'B' -> Upper
                'R' -> Upper
                _ -> error "Oops!"

getRowColumn :: BTree -> BTree -> String -> (Int, Int)
getRowColumn tree127 tree7 seat = (traverseTree tree127 row, traverseTree tree7 col)
  where
        (rowString, colString) = splitAt 7 seat
        row = map fbrlToSide rowString
        col = map fbrlToSide colString

seatId :: (Int, Int) -> Int
seatId (row, col) = row * 8 + col

part1 :: IO ()
part1 = do
  raw <- readFile "input.txt"

  let tree127 = makeTree [0..127]
  let tree7 = makeTree [0..7]

  let seats = map (seatId . getRowColumn tree127 tree7) $ lines raw

  print $ maximum seats

part2 :: IO ()
part2 = do
  raw <- readFile "input.txt"

  let tree127 = makeTree [0..127]
  let tree7 = makeTree [0..7]

  let seats = map (seatId . getRowColumn tree127 tree7) $ lines raw

  let seat = head $ [(minimum seats)..(maximum seats)] \\ seats -- Get the difference between the two lists

  print seat
