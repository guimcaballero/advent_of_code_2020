module Lib
    ( someFunc
    ) where

import Text.ParserCombinators.Parsec
import Data.List (foldl')
import Data.Bits

import Data.Map (Map)
import qualified Data.Map as Map


someFunc :: IO ()
someFunc = part2

data MaskItem = X | Zero | One
  deriving (Show, Eq)
type Mask = [MaskItem] -- ?
data Instruction = UpdateMask Mask | UpdateMemory Integer Integer
  deriving (Show, Eq)

-------------------
-- Parsing
-------------------

parseInstruction :: Parser Instruction
parseInstruction = do
  parseMem <|> parseMask

parseMem :: Parser Instruction
parseMem = do
  try $ string "mem"
  char '['
  adr <- read <$> many1 digit
  char ']'
  string " = "
  value <- read <$> many1 digit
  return $ UpdateMemory adr value

parseMask :: Parser Instruction
parseMask = do
  try $ string "mask"
  string " = "
  value <- many1 parseMaskItem
  return $ UpdateMask value

parseMaskItem :: Parser MaskItem
parseMaskItem = do
  (char 'X' >> return X) <|> (char '1' >> return One) <|> (char '0' >> return Zero)

unwrap (Right x) = x
unwrap _ = error "Unwrapping Left"

-------------------
-- Helpers
-------------------

-- Returns the mask with all 1 in X, and 0 for 1 and 0
binToInt :: [Integer] -> Integer
binToInt = foldl' (\acc x -> acc * 2 + x) 0

toBin :: Integer -> Bin
toBin 0 = [0]
toBin n = padToLength 36 $ reverse (helper n)
  where
    helper 0 = []
    helper n | n `mod` 2 == 1 = 1 : helper (n `div` 2)
            | n `mod` 2 == 0 = 0 : helper (n `div` 2)

padToLength len list
  | length list == len = list
  | otherwise = padToLength len $ 0:list

andMask :: Mask -> Integer
andMask mask = binToInt $ map fun mask
  where fun X = 1
        fun _ = 0

orMask :: Mask -> Integer
orMask mask = binToInt $ map fun mask
  where fun One = 1
        fun _ = 0

applyMask1 mask val = (.|.) ((.&.) val $ andMask mask) (orMask mask)

-- Returns all the integers after applying the mask
applyMask2 :: Mask -> Integer -> [Integer]
applyMask2 mask adr = map binToInt $ mask2Helper (zip (toBin adr) mask) [[]]

type Bin = [Integer]
mask2Helper :: [(Integer, MaskItem)] -> [Bin] -> [Bin]
mask2Helper [] accum = accum
mask2Helper (x:xs) accum = case x of
                             (val, Zero) -> mask2Helper xs $ map (\x -> x ++ [val]) accum
                             (_, One) -> mask2Helper xs $ map (\x -> x ++ [1]) accum
                             (_, X) -> mask2Helper xs $ (map (\x -> x ++ [1]) accum) ++ (map (\x -> x ++ [0]) accum)

-------------------
-- Main logic
-------------------

type Memory = Map Integer Integer
type Program = (Mask, Memory, [Instruction])

runProgram1 :: Program -> Program
runProgram1 (currentMask, memory, instructions)
  | null instructions = (currentMask, memory, [])
  | otherwise =
    case head instructions of
      UpdateMask mask -> runProgram1 (mask, memory, tail instructions)
      UpdateMemory adr val ->
        runProgram1 (currentMask, Map.insert adr (applyMask1 currentMask val) memory, tail instructions)


part1 :: IO ()
part1 = do
  raw <- lines <$> readFile "input.txt"
  let instructions = map (unwrap . parse parseInstruction "line") raw
  let (_, memory, _) = runProgram1 ([], Map.empty, instructions)
  print $ sum $ Map.elems memory


runProgram2 :: Program -> Program
runProgram2 (currentMask, memory, instructions)
  | null instructions = (currentMask, memory, [])
  | otherwise =
    case head instructions of
      UpdateMask mask -> runProgram2 (mask, memory, tail instructions)
      UpdateMemory adr val ->
        runProgram2 (currentMask, newMemory memory adr val, tail instructions)
  where
    newMemory memory adr val = foldr (\new accum -> Map.insert new val accum) memory (applyMask2 currentMask adr)


part2 :: IO ()
part2 = do
  raw <- lines <$> readFile "input.txt"
  let instructions = map (unwrap . parse parseInstruction "line") raw
  let (_, memory, _) = runProgram2 ([], Map.empty, instructions)
  print $ sum $ Map.elems memory
