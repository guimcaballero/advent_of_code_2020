{-# Language DeriveFunctor #-}

module Lib
    ( someFunc
    ) where

import Data.List
import Data.List.Extra (groupOn, splitOn)

someFunc :: IO ()
someFunc = do
  [deck1, deck2] <- map (map read . tail) . splitOn [""] . lines <$> readFile "input.txt"
  print $ part1 deck1 deck2
  print $ part2 deck1 deck2

type Tracker = [([Int], [Int])]
data Player a = One a | Two a
  deriving (Show, Eq, Functor)

deckScore :: [Int] -> Int
deckScore = sum . zipWith (*) [1..] . reverse

playUntilEnd :: [Int] -> [Int] -> Player [Int]
playUntilEnd deck [] = One deck
playUntilEnd [] deck = Two deck
playUntilEnd (a:as) (b:bs)
  | a > b = playUntilEnd (as ++ [a,b]) bs
  | otherwise = playUntilEnd as (bs ++ [b, a])

part1 :: [Int] -> [Int] -> Player Int
part1 = ((deckScore <$>) .) . playUntilEnd

playRecursive :: [Int] -> [Int] -> Tracker -> Player [Int]
playRecursive [] deck _ = Two deck
playRecursive deck [] _ = One deck
playRecursive (a:as) (b:bs) tracker
  | (a:as, b:bs) `elem` tracker = One (a:as)
  | a <= length as && b <= length bs = case playRecursive (take a as) (take b bs) [] of
                                      One _ -> playRecursive (as ++ [a, b]) bs newTracker
                                      Two _ -> playRecursive as (bs ++ [b, a]) newTracker
  | a > b = playRecursive (as ++ [a,b]) bs newTracker
  | otherwise = playRecursive as (bs ++ [b, a]) newTracker
  where
    newTracker = (a:as, b:bs):tracker

part2 :: [Int] -> [Int] -> Player Int
part2 deck1 deck2 = deckScore <$> playRecursive deck1 deck2 []
