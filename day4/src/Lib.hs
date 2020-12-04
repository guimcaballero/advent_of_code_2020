module Lib
    ( someFunc
    ) where

import Data.List (sort)
import Text.Regex.Posix

someFunc :: IO ()
someFunc = part2

splitIntoGroups :: [String] -> [[String]]
splitIntoGroups [] = [[]]
splitIntoGroups s = cons (case break (== "") s of
                                    (l, s') -> (l, case s' of
                                                    []      -> []
                                                    _:s''   -> splitIntoGroups s''))
  where
    cons ~(h, t)        =  h : t

wordsWhen     :: String -> [String]
wordsWhen s = case dropWhile (==':') s of
                      "" -> []
                      s' -> w : wordsWhen s''
                            where (w, s'') = break (==':') s'

listIntoTuple :: [String] -> (String, String)
listIntoTuple [a, b] = (a,b)
listIntoTuple _ = ("", "")

isCredential :: [String] -> [String] -> Bool
isCredential fields group = fields == group

part1 :: IO ()
part1 = do
  raw <- readFile "input.txt"
  let file = splitIntoGroups $ lines raw

  let wordGroups = map (map words) file
  let groups = map (sort . filter (/="cid") . map (head . wordsWhen) . concat) wordGroups

  let requiredFields = sort ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

  let credentials = length $ filter (isCredential requiredFields) groups

  print credentials


    -- byr (Birth Year) - four digits; at least 1920 and at most 2002.
    -- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    -- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    -- hgt (Height) - a number followed by either cm or in:
    --     If cm, the number must be at least 150 and at most 193.
    --     If in, the number must be at least 59 and at most 76.
    -- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    -- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    -- pid (Passport ID) - a nine-digit number, including leading zeroes.
    -- cid (Country ID) - ignored, missing or not.


fieldIsValid :: (String, String) -> Bool
fieldIsValid ("byr", val) = year >= 1920 && year <= 2002
  where year = read val :: Int
fieldIsValid ("iyr", val) = year >= 2010 && year <= 2020
  where year = read val :: Int
fieldIsValid ("eyr", val) = year >= 2020 && year <= 2030
  where year = read val :: Int
fieldIsValid ("hgt", val) = case unit of
                              "cm" -> height >= 150 && height <= 193
                              "in" -> height >= 59 && height <= 76
                              _ -> False
  where idx = length val - 2
        (heightString, unit) = splitAt idx val
        height = read heightString :: Int
fieldIsValid ("hcl", val) = val =~ "#[a-f0-9]{6}"
fieldIsValid ("ecl", val) = val `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
fieldIsValid ("pid", val) = length val == 9

fieldIsValid _ = True

validateCredential group = fieldsAreValid && allFieldsPresent
  where
    requiredFields = sort ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    allFieldsPresent = requiredFields == (sort $ filter (/= "cid") $ map fst group)
    fieldsAreValid = and $ map fieldIsValid group

part2 :: IO ()
part2 = do
  raw <- readFile "input.txt"
  let file = splitIntoGroups $ lines raw

  let wordGroups = map (map words) file
  let groups = map (map (listIntoTuple . wordsWhen) . concat) wordGroups

  let credentials = length $ filter validateCredential groups
  print credentials
