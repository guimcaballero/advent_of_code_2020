{-# LANGUAGE Strict, TupleSections #-}

module Lib
    ( someFunc
    ) where

import Data.List
import Data.Maybe

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

someFunc :: IO ()
someFunc = part2

-- Slow naive implementation with recursion
genUpToNumber :: Int -> [Int] -> [Int]
genUpToNumber num list
  | length list == num = list
  | otherwise = genUpToNumber num $ val : list
  where
    val =
      case elemIndex (head list) (tail list) of
        Nothing -> 0
        Just idx -> idx + 1

part1 :: IO ()
part1 = do
  let list = [14,3,1,0,9,5] :: [Int]
  print list

  let res = head $ genUpToNumber 2020 $ reverse list
  print res

-- Faster implementation using IntMap
getNumber :: Int -> Int -> Int -> IntMap (Int, Int) -> Int
getNumber num iteration list tracker
  | iteration == num = list
  | otherwise = getNumber num (iteration + 1) val newTracker
  where
    val = case IntMap.lookup list tracker of
      Nothing -> 0
      Just (a, b) -> if b == -1 then 0 else iteration - b

    updateVal (Just (a, b)) = Just (iteration + 1, a)
    updateVal Nothing = Just (iteration + 1, -1)
    newTracker = IntMap.alter updateVal val tracker

part2 :: IO ()
part2 = do
  let list = [14, 3, 1, 0, 9, 5] :: [Int]
  let tracker = IntMap.fromList [val | val <- zip list $ map (,-1) [1..]]
  print tracker

  print $ getNumber 30000000 (length list) (head $ reverse list) tracker
