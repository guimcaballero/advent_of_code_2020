module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
  trees <- lines <$> readFile "input.txt"
  part1 trees
  part2 trees

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


part1, part2 :: [String] -> IO ()
part1 trees = do
  print $ countTreesInSlope trees (1, 3)
part2 trees = do
  let slopes = [(1, 1),(1, 3),(1, 5),(1, 7),(2, 1)]
  print $ product $ map (countTreesInSlope trees) slopes
