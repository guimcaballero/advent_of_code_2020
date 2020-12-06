module Lib
    ( someFunc
    ) where

import Data.List

someFunc :: IO ()
someFunc = part2

splitIntoGroups :: [String] -> [[String]]
splitIntoGroups [] = [[]]
splitIntoGroups s = cons (case break (== "") s of
                                    (l, s') -> (l, case s' of
                                                    []      -> []
                                                    _:s''   -> splitIntoGroups s''))
  where
    cons ~(h, t)        =  h : t

part1 :: IO ()
part1 = do
  raw <- readFile "input.txt"
  let groups = splitIntoGroups $ lines raw

  let questions = map (length . nub . concat) groups

  print $ sum questions


answeredQuestionsInGroup :: [String] -> Int
answeredQuestionsInGroup group = length $ foldr commonElt ['a'..'z'] group

commonElt s1 s2 = filter (`elem` s2) s1

part2 :: IO ()
part2 = do
  raw <- readFile "input.txt"
  let groups = splitIntoGroups $ lines raw

  let questions = map answeredQuestionsInGroup groups

  print $ foldr commonElt ['a'..'z'] ["asdf", "as", "fdsa", "tas"]

  print $ sum questions
