module Lib
    ( someFunc
    ) where

someFunc = part2

checkIfPositionIsTree :: [String] -> (Int, Int) -> Bool
checkIfPositionIsTree trees (row, column) = '#' == (trees !! row) !! column

countTreesInSlope :: [String] -> (Int, Int) -> Int
countTreesInSlope trees (x, y) = length treePositions
  where
      verticalLength = length trees
      horizontalLength = length $ head trees -- Assumes all strings are the same length
      -- Make an array of all of the positions to check
      sloper s = (s * x, (s * y) `mod` horizontalLength)
      positions = map sloper [0..(verticalLength - 1) `div` x]
      -- Get the ones that have a tree
      treePositions = filter (checkIfPositionIsTree trees) positions


part1 :: IO ()
part1 = do
  raw <- readFile "input.txt"

  let trees = lines raw
  print $ countTreesInSlope trees (1, 3)

part2 :: IO ()
part2 = do
  raw <- readFile "input.txt"

  let trees = lines raw

  let slopes = [(1, 1),(1, 3),(1, 5),(1, 7),(2, 1)]
  let counts = map (countTreesInSlope trees) slopes

  print $ product counts
