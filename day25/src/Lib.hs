module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
  let card = 9717666
  let door = 20089533

  print $ part1 card door

loopNum :: Int -> Int -> Int -> Int -> Int
loopNum its subj val target
  | val == target = its
  | otherwise = loopNum (its + 1) subj ((subj * val) `mod` 20201227) target

loopIters :: Int -> Int -> Int -> Int -> Int
loopIters its subj val target
  | its == target = val
  | otherwise = loopIters (its + 1) subj ((subj * val) `mod` 20201227) target


part1 :: Int -> Int -> Int
part1 card door = loopIters 0 door 1 cardLoop
  where
    cardLoop = loopNum 0 7 1 card
