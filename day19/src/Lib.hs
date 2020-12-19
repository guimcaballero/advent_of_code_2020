module Lib
    ( someFunc
    ) where

import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec

import Data.IntMap (IntMap)
import qualified Data.IntMap as Map

someFunc :: IO ()
someFunc = part1 >> part2

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

parseRule :: Parser (Int, Rule)
parseRule = do
  number <- read <$> many1 digit
  string ": "
  rule <- try parseCharRule <|> try parseOrRule <|> parseSubRules
  return (number, rule)

parseCharRule :: Parser Rule
parseCharRule = do
  char '"'
  actualChar <- letter
  char '"'

  return $ CharRule actualChar

parseSubRules :: Parser Rule
parseSubRules = do
  side <- map (RulePointer . read) . words <$> many1 (digit <|> char ' ')
  return $ SubRules side

parseOrRule :: Parser Rule
parseOrRule = do
  side1 <- parseSubRules
  char '|'
  OrRule side1 <$> parseSubRules

fromRight (Right x) = x
fromRight _ = error "Unwrapping Left"

unwrap (Just x) = x
unwrap _ = error "Unwrapping Nothing"

-------------------
-- Types
-------------------

data Rule
  = CharRule Char
  | SubRules [Rule]
  | OrRule Rule Rule
  | RulePointer Int
  deriving (Show, Eq)

type Rules = IntMap Rule

-------------------
-- Main logic
-------------------

checkStringMatchesRule :: Rules -> Rule -> String -> [String]
checkStringMatchesRule rules _ "" = []
checkStringMatchesRule rules rule string =
  case rule of
    RulePointer i -> checkStringMatchesRule rules (rules Map.! i) string
    CharRule a ->
      if a == head string
        then [tail string]
        else []
    SubRules sub -> (foldr (>=>) pure) (checkStringMatchesRule rules <$> sub) string
    OrRule rule1 rule2 -> concat [through1, through2]
      where through1 = checkStringMatchesRule rules rule1 string
            through2 = checkStringMatchesRule rules rule2 string

match :: Rules -> Rule -> String -> Bool
match rules rule string = not $ null $ filter null $ checkStringMatchesRule rules rule string

part1 :: IO ()
part1 = do
  [rulesRaw, messages] <- splitOn (== "") . lines <$> readFile "input.txt"

  let rules = Map.fromList $ map (fromRight . parse parseRule "rules") rulesRaw
  let rule0 = rules Map.! 0

  print $ length $ filter (match rules rule0) messages

part2 :: IO ()
part2 = do
  [rulesRaw, messages] <- splitOn (== "") . lines <$> readFile "input.txt"

  let normalRules = Map.fromList $ map (fromRight . parse parseRule "rules") rulesRaw
  let rules = Map.insert 8 (OrRule (RulePointer 42) (SubRules [RulePointer 42, RulePointer 8]))
                $ Map.insert 11 (OrRule (SubRules [RulePointer 42, RulePointer 31]) (SubRules [RulePointer 42, RulePointer 11, RulePointer 31])) normalRules

  let rule0 = rules Map.! 0

  print $ length $ filter (match rules rule0) messages
