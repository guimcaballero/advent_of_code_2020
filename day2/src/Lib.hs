module Lib
    ( someFunc
    ) where

import Text.Regex.Posix

someFunc = part2

parseLine :: String -> (Int, Int, Char, String)
parseLine line = (read min, read max, head letter, password)
  where [min, max, letter, password] = match
        (_,_,_,match) = line =~ "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)" :: (String, String, String, [String])

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

checkPolicy1 :: (Int, Int, Char, String) -> Bool
checkPolicy1 (min, max, letter, password) = min <= len && len <= max
  where len = count letter password

part1 :: IO ()
part1 = do
  raw <- readFile "input.txt"

  let vec = lines raw
  let parsedVec = map parseLine vec

  let filteredVec = filter checkPolicy1 parsedVec

  print $ length filteredVec

xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a

checkPolicy2 :: (Int, Int, Char, String) -> Bool
checkPolicy2 (low, high, letter, password) = xor a b
  where a = (password !! (low - 1)) == letter
        b = (password !! (high - 1)) == letter

part2 :: IO ()
part2 = do
  raw <- readFile "input.txt"

  let vec = lines raw
  let parsedVec = map parseLine vec

  let filteredVec = filter checkPolicy2 parsedVec

  print $ length filteredVec
