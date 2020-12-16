module Lib
    ( someFunc
    ) where

import Data.List
import Text.ParserCombinators.Parsec

import Data.Map (Map)
import qualified Data.Map as Map

someFunc :: IO ()
someFunc = part2

-------------------
-- Parsing
-------------------

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn p s = cons (case break p s of
                                    (l, s') -> (l, case s' of
                                                    []      -> []
                                                    _:s''   -> splitOn p s''))
  where
    cons ~(h, t)        =  h : t

splitIntoGroups ::[String] -> [[String]]
splitIntoGroups = splitOn (== "")
splitIntoNums ::String -> [String]
splitIntoNums = splitOn (== ',')

parseRules :: Parser Rule
parseRules = do
  name <- many1 (letter <|> char ' ')
  string ": "

  number1 <- read <$> many1 digit
  char '-'
  number2 <- read <$> many1 digit
  string " or "

  number3 <- read <$> many1 digit
  char '-'
  number4 <- read <$> many1 digit

  return (name, [(number1, number2), (number3, number4)])

unwrap (Right x) = x
unwrap _ = error "Unwrapping Left"

unwrapMaybe (Just x) = x
unwrapMaybe _ = error "Unwrapping Nothing"

-----------------
-- Main logic
-----------------

type Ticket = [Int]
type Rule = (String, [(Int, Int)])

fieldIsValidForRule :: [(Int, Int)] -> Int -> Bool
fieldIsValidForRule xs field = or $ map (\(a, b) -> (a <= field) && (field <= b)) xs

fieldIsValidForSomeRules :: [Rule] -> Int -> Bool
fieldIsValidForSomeRules rules field = or $ map fun rules
  where
    fun (_, nums) = fieldIsValidForRule nums field

invalidFieldsInTicket :: [Rule] -> Ticket -> [Int]
invalidFieldsInTicket rules ticket = [ field | field <- ticket, not $ fieldIsValidForSomeRules rules field]

part1 :: IO ()
part1 = do
  [rawRules, myTicket, otherTickets] <- splitIntoGroups . lines <$> readFile "input.txt"
  let rules = map (unwrap . parse parseRules "rules") rawRules

  let others = map (map read . splitIntoNums) $ tail otherTickets :: [[Int]]

  let errors = map (invalidFieldsInTicket rules) others
  -- print errors
  print $ sum $ concat errors


-- Keeps a map of index possibilities for each rule
type Tracker = Map String [Int]
type Rules = Map String [(Int, Int)]

updateTrackerWithTickets :: Rules -> [Ticket] -> Tracker -> Tracker
updateTrackerWithTickets rules [] tracker = tracker
updateTrackerWithTickets rules (ticket:xs) tracker = updateTrackerWithTickets rules xs newTracker
  where
    newTracker = Map.mapWithKey updateTracker tracker
    updateTracker key positions = filter (\position -> fieldIsValidForRule (unwrapMaybe $ Map.lookup key rules) (ticket !! position)) positions

solveTracker :: Tracker -> Tracker
solveTracker tracker
  | null $ Map.filter (\positions -> length positions > 1) tracker = tracker
  | otherwise = solveTracker newTracker
  where
    -- Get the ones that have only one item
    singles = concat $ Map.elems $ Map.filter (\positions -> length positions == 1) tracker
    -- Only filter the ones that have more than one item
    newTracker = Map.map help tracker
    help (a:[]) = [a]
    help positions = filter (not . flip elem singles) positions

multiplyDepartures :: Tracker -> Ticket -> Int
multiplyDepartures tracker ticket = product $ map (ticket !!) indexes
  where
    help key value = isPrefixOf "departure" key
    indexes = map head $ Map.elems $ Map.filterWithKey help tracker

part2 :: IO ()
part2 = do
  [rawRules, myTicket, otherTickets] <- splitIntoGroups . lines <$> readFile "input.txt"

  let rules = map (unwrap . parse parseRules "rules") rawRules
  let others = map (map read . splitIntoNums) $ tail otherTickets :: [[Int]]
  let ticket = map read . splitIntoNums $ last myTicket :: [Int]

  -- Get the tickets that are valid
  let validTickets = ticket:filter (null . invalidFieldsInTicket rules) others

  let tracker = Map.fromList $ map (\rule -> (fst rule, [0..length rules - 1])) rules
  let final = updateTrackerWithTickets (Map.fromList rules) validTickets tracker
  print final
  let solved = solveTracker final
  print solved

  print $ multiplyDepartures solved ticket
