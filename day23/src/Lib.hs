module Lib
    ( someFunc
    ) where

import Data.List
import Control.Monad.ST
import Control.Monad (foldM_, zipWithM_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (Ix, MArray, STUArray, getBounds, newArray_, readArray, writeArray)

someFunc :: IO ()
someFunc = do
  let numbers = read . return <$> "974618352"
  print $ part1 numbers
  print $ part2 $ numbers ++ [10..1000000]

getDest selected rest
  | selected < minimum rest = maximum rest
  | otherwise = if poss `elem` rest then poss else getDest poss rest
  where
    poss = selected - 1

addABCAfterDest [a, b, c] dest rest = concat $ map help rest
  where
    help x = if x == dest then [x, a, b, c] else [x]

playRound :: [Int] -> [Int]
playRound (selected:a:b:c:rest) = newList ++ [selected]
  where
    dest = getDest selected rest
    newList = addABCAfterDest [a, b, c] dest rest
playRound _ = error "no"

circleTillFirst :: [Int] -> [Int]
circleTillFirst (1:rest) = 1:rest
circleTillFirst (a:rest) = circleTillFirst (rest ++ [a])
circleTillFirst _ = error "no"

part1 :: [Int] -> String
part1 cups = concatMap show $ tail $ circleTillFirst $ go 0 cups
  where
    go 100 cs = cs
    go i cs = go (i + 1) $ playRound cs


-- Not happy with this, I don't know enough about mutable arrays in haskell and don't have enough time to learn, so I had to take inspiration from other solutions

part2 :: [Int] -> Int
part2 input@(x:_) = runST $ do
    arr <- newArray' input
    foldM_ (const . step arr) x $ replicate 10000000 ()
    y <- readArray arr 1
    z <- readArray arr y
    pure $ y * z

newArray' :: [Int] -> ST s (STUArray s Int Int)
newArray' xs = do
    arr <- newArray_ (minimum xs, maximum xs)
    arr <$ zipWithM_ (writeArray arr) xs (drop 1 $ cycle xs)

playRoundA :: (MArray a e m, Enum e, Ix e) => a e e -> e -> m e
playRoundA arr x = do
    (lo, hi) <- getBounds arr
    a <- readArray arr x
    b <- readArray arr a
    c <- readArray arr b
    y <- readArray arr c
    let pred' z = if z == lo then hi else pred z
        t:_ = dropWhile (`elem` [a, b, c]) . iterate pred' $ pred' x
    u <- readArray arr t
    writeArray arr x y
    writeArray arr t a
    writeArray arr c u
    pure y
