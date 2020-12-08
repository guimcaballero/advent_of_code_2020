module Lib
    ( someFunc
    ) where

import Data.List

someFunc :: IO ()
someFunc = part2

data Operation = Acc | Nop | Jmp
  deriving (Show, Eq)

-- Operation, Argument, Read?
type Instruction = (Operation, Int, Bool)

-- Current line, Accumulator, Instructions, Has changed instruction, Previous line
type Holder = (Int, Int, [Instruction], Bool)

parseInt :: String -> Int
parseInt val = case head val of
                 '+' -> read $ tail val
                 _ -> read val

parseOperation :: String -> Operation
parseOperation val = case val of
                     "acc" -> Acc
                     "jmp" -> Jmp
                     "nop" -> Nop
                     _ -> error "Oops!"

parseLine :: [String] -> Instruction
parseLine [op, argument] = (parseOperation op, parseInt argument, False)
parseLine _ = error "Oops!"


replaceLine :: [a] -> a -> Int -> [a]
replaceLine list newLine idx = x ++ newLine : ys
  where (x, _:ys) = splitAt idx list

-- Gets the holder. Reads lines until one is repeated, then returns acc
readInstructions :: Holder -> Int
readInstructions (line, acc, instructions, changes) =
  if hasBeenRead
    then acc
    else case op of
           Nop ->
             readInstructions (line + 1, acc, newInstructions, changes)
           Acc ->
             readInstructions
               (line + 1, acc + arg, newInstructions, changes)
           Jmp ->
             readInstructions (line + arg, acc, newInstructions, changes)
  where
    (op, arg, hasBeenRead) = instructions !! line
    newInstruction = (op, arg, True)
    newInstructions = replaceLine instructions newInstruction line

part1 :: IO ()
part1 = do
  instructions <- map (parseLine . words) . lines <$> readFile "input.txt"

  -- let instructions = map parseLine raw
  let holder = (0, 0, instructions, False)

  let final = readInstructions holder

  print final

-- Gets the holder. Reads lines until one is repeated, then returns acc
readInstructions2 :: Holder -> Maybe Int
readInstructions2 (line, acc, instructions, changed)
  -- If it's the line after the last one, return the accumulator
  | line == length instructions = Just acc
  -- If it's over the one after the last one, return Nothing
  | line > length instructions = Nothing
  -- If this line has been read we go back
  | hasBeenRead = Nothing
  -- If we have already changed a line, run normally
  | changed = case op of
           Nop ->
             readInstructions2 (line + 1, acc, newInstructions, changed)
           Acc ->
             readInstructions2
               (line + 1, acc + arg, newInstructions, changed)
           Jmp ->
             readInstructions2 (line + arg, acc, newInstructions, changed)
  -- If not, we go down both paths
  | otherwise =
    case op of
           Acc -> readInstructions2 (line + 1, acc + arg, newInstructions, changed)
           _ -> case (treatAsNop, treatAsJump) of
             (Just accum, Nothing) -> Just accum
             (Nothing, Just accum) -> Just accum
             (Nothing, Nothing) -> Nothing
             _ -> error "Both paths can't lead to Just. Something is wrong"
  where
    (op, arg, hasBeenRead) = instructions !! line
    newInstruction = (op, arg, True)
    newInstructions = replaceLine instructions newInstruction line

    treatAsNop = readInstructions2 (line + 1, acc, newInstructions, op /= Nop)
    treatAsJump = readInstructions2 (line + arg, acc, newInstructions, op /= Jmp)

part2 :: IO ()
part2 = do
  instructions <- map (parseLine . words) . lines <$> readFile "input.txt"
  let holder = (0, 0, instructions, False)

  let final = readInstructions2 holder
  print final
