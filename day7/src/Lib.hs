module Lib
    ( someFunc
    ) where

import Data.List

someFunc :: IO ()
someFunc = part2

getRules :: [String] -> Rule
getRules line = (color, inner)
  where
    (a, b) = break (== "bags") line
    color = unwords a
    (inner, _) = matchInnerRules [] $ drop 2 b

matchInnerRules :: [(Int, String)] -> [String] -> ([(Int, String)], [String])
matchInnerRules array rest = do
    let (a, b) = break (\s -> s == "bags," || s == "bags." || s == "bag," || s == "bag.") rest

    if unwords a == "no other" then (array, []) else do
      let newArray = (read $ head a, unwords (tail a)):array
      let newRest = tail b

      if not $ null newRest then
          matchInnerRules newArray newRest
      else
          ( newArray, newRest )

type Map a = [(String, a)]
type Rule = (String, [(Int, String)])
type Rules = [Rule]

getRuleForColor :: Rules -> String -> Rule
getRuleForColor rules color = head $ filter ((== color) . fst) rules

canContainBag ::  String -> Rules -> Rule -> Map Bool -> Map Bool
canContainBag bag rules (color, inner) colors = do
  -- If this color is in the list, just return it, if not, check it
  let inList = any ((== color) . fst) colors

  if inList then colors else do
    let canImmediatelyHave = elem bag $ map snd inner
    if canImmediatelyHave || null inner then (color, canImmediatelyHave):colors else do
      let newColors = foldr (canContainBag bag rules . getRuleForColor rules) colors $ map snd inner
      let canHaveSub = any (snd . head . \new -> filter ((== new) . fst) newColors) $ map snd inner
      (color, canHaveSub):newColors


part1 :: IO ()
part1 = do
  raw <- lines <$> readFile "input.txt"

  let rules = map (getRules . words) raw


  let bag = "shiny gold"

  let fun = canContainBag bag rules
  let bagsThatCanContain = length $ filter snd $ foldr fun [] rules

  print bagsThatCanContain

bagsNeededForBag :: Rules -> Rule -> Map Int -> Map Int
bagsNeededForBag rules (color, inner) colors = do
  -- If this color is in the list, just return it, if not, check it
  let inList = any ((== color) . fst) colors

  if inList then colors else
    if null inner then (color, 1):colors else do
      let newColors = foldr ((bagsNeededForBag rules . getRuleForColor rules) . snd) colors inner
      let num = sum $ map (test newColors) inner
      (color, 1 + num):newColors

test :: Map Int -> (Int, String) -> Int
test newColors (n, string) = n * snd (head $ filter ((== string) . fst) newColors)

part2 :: IO ()
part2 = do
  raw <- lines <$> readFile "input.txt"

  let rules = map (getRules . words) raw

  let bag = getRuleForColor rules "shiny gold"
  -- print $ snd bag

  let bags = bagsNeededForBag rules bag []

  print bags

  let gold = head $ filter ((== "shiny gold") . fst) bags

  print $ snd gold - 1
