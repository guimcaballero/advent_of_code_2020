module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = part2

checkNextNumberIsAdditionOfPrevious :: [Int] -> Int
checkNextNumberIsAdditionOfPrevious numbers =
  if not $ null results then checkNextNumberIsAdditionOfPrevious $ drop 1 numbers else
    number

  where
    initial = take 25 numbers
    number = numbers !! 25
    results = [(i, j) | i <- initial,
                       j <- initial,
                       i + j == number]


part1 :: IO ()
part1 = do
  raw <- lines <$> readFile "input.txt"
  let numbers = map read raw :: [Int]

  print $ checkNextNumberIsAdditionOfPrevious numbers

addToNumberOrNothing :: Int -> [Int] -> [Int] -> Maybe [Int]
addToNumberOrNothing number numbers accumList
  | val > number = Nothing
  | val == number = Just accumList
  | otherwise = addToNumberOrNothing number (drop 1 numbers) $ head numbers:accumList
  where
    val = sum accumList

findContiguousNumbersThatAddUp :: Int -> [Int] -> [Int]
findContiguousNumbersThatAddUp number numbers =
  case addToNumberOrNothing number numbers [] of
    Just array -> array
    Nothing -> findContiguousNumbersThatAddUp number $ drop 1 numbers

part2 :: IO ()
part2 = do
  raw <- lines <$> readFile "input.txt"
  let numbers = map read raw :: [Int]

  let number = checkNextNumberIsAdditionOfPrevious numbers
  let list = findContiguousNumbersThatAddUp number numbers
  let addition = minimum list + maximum list

  print addition
