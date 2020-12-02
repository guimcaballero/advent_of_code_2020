module Lib
    ( someFunc
    ) where

someFunc = part2

printResult1 :: (Show a, Num a) => [(a, a)] -> IO ()
printResult1 [(i, j)] = print $ i * j
printResult1 _ = print "No result found"

printResult2 :: (Show a, Num a) => [(a, a, a)] -> IO ()
printResult2 [(i, j, k)] = print $ i * j * k
printResult2 _ = print "No result found"

part1 :: IO ()
part1 = do
  raw <- readFile "input.txt"
  let vec = lines raw
  let nums = map read vec :: [Int]

  let result = take 1 [(i, j) | i <- nums,
                       j <- nums,
                       i + j == 2020]

  printResult1 result

part2 :: IO ()
part2 = do
  raw <- readFile "input.txt"
  let vec = lines raw
  let nums = map read vec :: [Int]

  let result = take 1 [(i, j, k) | 
                        i <- nums,
                        j <- nums,
                        k <- nums,
                        (i + j + k) == 2020]

  printResult2 result
