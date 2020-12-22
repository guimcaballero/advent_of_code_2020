module Lib
    ( someFunc
    ) where

import Data.List
import Data.List.Extra (groupOn, splitOn)

someFunc :: IO ()
someFunc = part2

playUntilEnd :: [Int] -> [Int] -> ([Int], [Int])
playUntilEnd deck1 [] = (deck1, [])
playUntilEnd [] deck2 = (deck2, [])
playUntilEnd (a:as) (b:bs) = playUntilEnd newA newB
  where
    (newA, newB) = if a > b then (as ++ [a, b], bs) else (as, bs ++ [b, a])

deckScore = sum . zipWith (*) [1..] . reverse

part1 :: IO ()
part1 = do
  [deck1, deck2] <- map (map read . tail) . splitOn [""] . lines <$> readFile "input2.txt"
  let end = fst $ playUntilEnd deck1 deck2
  print $ deckScore end


type Tracker = [([Int], [Int])]
data Player a = One a | Two a
  deriving (Show, Eq)

playRecursive :: [Int] -> [Int] -> Tracker -> Player [Int]
playRecursive [] deck _ = Two deck
playRecursive deck [] _ = One deck
playRecursive (a:as) (b:bs) tracker
  | (a:as, b:bs) `elem` tracker = One (a:as)
  | a <= length as && b <= length bs = case playRecursive (take a as) (take b bs) [] of
                                      One _ -> playRecursive (as ++ [a, b]) bs newTracker
                                      Two _ -> playRecursive as (bs ++ [b, a]) newTracker
  | otherwise = playRecursive newA newB newTracker
  where
    (newA, newB) = if a > b then (as ++ [a, b], bs) else (as, bs ++ [b, a])
    newTracker = (a:as, b:bs):tracker

playGame :: [Int] -> [Int] -> Int
playGame deck1 deck2 = case playRecursive deck1 deck2 [] of
                         One deck -> deckScore deck
                         Two deck -> deckScore deck

part2 :: IO ()
part2 = do
  [deck1, deck2] <- map (map read . tail) . splitOn [""] . lines <$> readFile "input.txt"
  print $ playGame deck1 deck2
