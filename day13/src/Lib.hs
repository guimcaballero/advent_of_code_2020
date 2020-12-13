module Lib
    ( someFunc
    ) where

import Data.List
import Data.Function

import Math.NumberTheory.Moduli.Chinese

someFunc :: IO ()
someFunc = do
  raw <- lines <$> readFile "input.txt"

  putStrLn ""
  putStrLn "PART 1:"
  part1 raw

  putStrLn ""
  putStrLn "-------"
  putStrLn "PART 2:"
  part2 raw

splitComma   :: String -> [String]
splitComma s =  case dropWhile (==',') s of
                      "" -> []
                      s' -> w : splitComma s''
                            where (w, s'') = break (== ',') s'

data Id = BusId Integer | X
  deriving (Show, Eq)

fromId (BusId n) = n
fromId X = error "X passed to fromId"

toId "x" = X
toId n = BusId $ read n

parseIds :: String -> [Id]
parseIds ids = map toId $ splitComma ids
  where

generateTimeTableForId :: Integer -> [Integer]
generateTimeTableForId busId = [i * busId | i <- [0..]]

getEarliestDepartAfterTime :: Integer -> Integer -> Integer
getEarliestDepartAfterTime time busId = head [ i | i <- generateTimeTableForId busId, i >= time ]

getEarliestBusAfterTime :: [Id] -> Integer -> (Integer, Integer)
getEarliestBusAfterTime busIds time = minimumBy (compare `on` snd) busTimes
  where
    ids = map fromId busIds
    busTimes = map (\busId -> (busId, getEarliestDepartAfterTime time busId)) ids

part1 :: [String] -> IO ()
part1 [timeString, idsString] = do
  let time = read timeString :: Integer
  let ids = filter (/= X) $ parseIds idsString

  let (bus, departTime) = getEarliestBusAfterTime ids time

  print $ bus * (departTime - time)

part2 :: [String] -> IO ()
part2 [timeString, idsString] = do
  let time = read timeString :: Integer
  let ids = parseIds idsString
  print $ chineseRemainder $ map (\(idx, busId) -> (idx, fromId busId)) $ filter ((/= X) . snd) $ zip [0, -1..] ids
