module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
  nums <- map read . lines <$> readFile "input.txt"
  print $ part1 nums
  print $ part2 nums

part1, part2 :: [Int] -> Int
part1 nums = head [i * j | i <- nums, j <- nums, i + j == 2020]
part2 nums = head [i * j * k | i <- nums, j <- nums, k <- nums, (i + j + k) == 2020]
