{-# Language TupleSections #-}

module Lib
    ( someFunc
    ) where

import Data.List
import Data.List.Extra (groupOn)

import Text.ParserCombinators.Parsec

import Data.Map (Map)
import qualified Data.Map as Map

someFunc :: IO ()
someFunc = part1 >> part2

-------------------
-- Parsing
-------------------

word = do
  x <- many1 letter
  many $ oneOf " \n\t,"
  return x

parseIngredientList :: Parser ([Ingredient], [Allergen])
parseIngredientList = do
  ingredients <- many1 word
  string "(contains "
  allergens <- many1 word
  char ')'
  return $ (allergens,ingredients)

fromRight (Right x) = x
fromRight _ = error "Unwrapping Left"

type Ingredient = String
type Allergen = String
type Tracker = Map Allergen [Ingredient]

oneIngredient :: Tracker -> [Ingredient]
oneIngredient = nub . concat . filter ((== 1) . length) . Map.elems

solveTracker tracker
  | null $ Map.filter ((> 1) . length) tracker = tracker
  | otherwise = solveTracker $ Map.map help tracker
  where
    singles = oneIngredient tracker

    help [a] = [a]
    help allergens = filter (not . flip elem singles) allergens

genTracker :: [([Allergen], [Ingredient])] -> Tracker
genTracker list = Map.fromList smth2
  where
    smth = concat $ map (\(a, b) -> map (, b) a) list
    keys = nub $ map fst smth
    smth1 = map (\key -> (key, map snd $ filter ((== key) . fst) smth)) keys
    smth2 = map (\(a, ingr) -> (a, foldr1 intersect ingr)) smth1

part1 :: IO ()
part1 = do
  raw <- lines <$> readFile "input.txt"
  let list = map (fromRight . parse parseIngredientList "ingredients") raw

  let solved = Map.map (head) $ solveTracker $ genTracker list

  let ingredientList = concat $ map snd list
  let allergicIngredients = Map.elems solved
  let nonAllergicIngredients = filter (not . (`elem` allergicIngredients)) ingredientList

  print $ length nonAllergicIngredients

part2 :: IO ()
part2 = do
  raw <- lines <$> readFile "input.txt"
  let list = map (fromRight . parse parseIngredientList "ingredients") raw

  let solved = Map.elems $ Map.map (head) $ solveTracker $ genTracker list
  print $ intercalate "," solved
