module Lib
    ( someFunc
    ) where

import Data.MemoTrie
import Data.Maybe
import Data.List

someFunc :: IO ()
someFunc = part2

calcDifferences :: (Int, Int) -> [Int] -> (Int, Int)
calcDifferences accum [] = accum
calcDifferences accum [_] = accum
calcDifferences (one, three) (x:y:xs) =
  case diff of
    1 -> calcDifferences (one + 1, three) $ y:xs
    3 -> calcDifferences (one, three + 1) $ y:xs
    _ -> calcDifferences (one, three) $ y:xs
  where
    diff = y - x

part1 :: IO ()
part1 = do
  raw <- lines <$> readFile "input.txt"
  let numbers = sort $ map read raw :: [Int]

  let maxNum = maximum numbers
  let numbers = 0 : numbers ++ [maxNum + 3]

  let (one, three) = calcDifferences (0, 0) numbers

  print (one, three)
  print $ one * three




calcMemo :: [Int] -> Int
calcMemo = memo calcPossibilities

calcPossibilities :: [Int] -> Int
calcPossibilities (x:[]) = 1
calcPossibilities (x:xs) = calcMemo (dropp 3) + calcMemo (dropp 2) + calcMemo (dropp 1)
  where
    dropp i = dropWhile (\y -> y - x /= i) xs
calcPossibilities [] = 0

part2 :: IO ()
part2 = do
  raw <- lines <$> readFile "input.txt"
  let base = sort $ map read raw :: [Int]
  let maxNum = maximum base
  let numbers = 0 : base ++ [maxNum + 3]

  print $ calcMemo numbers
