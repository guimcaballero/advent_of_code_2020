module Lib
    ( someFunc
    ) where

import Text.Regex.Posix

someFunc :: IO ()
someFunc = do
  raw <- map parseLine . lines <$> readFile "input.txt"
  print $ length $ filter checkPolicy1 raw
  print $ length $ filter checkPolicy2 raw

type Line = (Int, Int, Char, String)

parseLine :: String -> Line
parseLine line = (read mn, read mx, head letter, password)
  where
    (_, _, _, [mn, mx, letter, password]) =
      line =~ "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)" :: ( String
                                                       , String
                                                       , String
                                                       , [String])

count :: Eq a => a -> [a] -> Int
count a = length . filter (a ==)

checkPolicy1 :: (Int, Int, Char, String) -> Bool
checkPolicy1 (mn, mx, letter, password) = mn <= len && len <= mx
  where len = count letter password

xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a

checkPolicy2 :: (Int, Int, Char, String) -> Bool
checkPolicy2 (low, high, letter, password) = xor a b
  where a = (password !! (low - 1)) == letter
        b = (password !! (high - 1)) == letter
