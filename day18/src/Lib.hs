module Lib
  ( someFunc
  ) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language (haskellStyle)

someFunc :: IO ()
someFunc = part1 >> part2

-------------------
-- Parsing
-------------------

expr table = buildExpressionParser table (term table)
        <?> "expression"

lexer = makeTokenParser haskellStyle
term table =  parens lexer (expr table)
        <|> natural lexer
        <?> "simple expression"

tablePart1 = [[binary "+" (+) AssocLeft, binary "*" (*) AssocLeft]]
tablePart2 = [[binary "+" (+) AssocLeft], [binary "*" (*) AssocLeft]]

binary  name fun assoc = Infix (do{ reservedOp lexer name; return fun }) assoc

unwrap (Right x) = x
unwrap _ = error "Unwrapping Left"

-------------------
-- Main logic
-------------------

part1 :: IO ()
part1 = do
  raw <- lines <$> readFile "input.txt"
  let table = tablePart1
  let expressions = map (unwrap . parse (expr table) "Expressions") raw
  print $ sum expressions

part2 :: IO ()
part2 = do
  raw <- lines <$> readFile "input.txt"
  let table = tablePart2
  let expressions = map (unwrap . parse (expr table) "Expressions") raw
  print $ sum expressions
