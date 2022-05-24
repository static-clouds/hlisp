module Main where

import Text.Parsec hiding (digit)
import Text.Parsec.String (Parser, GenParser)

data Token = TPlus | TMinus | TNum Int | TExpr Token Token Token deriving Show

-- this was a sort of first go at making a parser,
-- i totally forgot how parsec worked >:)

plus :: GenParser Char () Token
plus = TPlus <$ string "+"

minus :: GenParser Char () Token
minus = TMinus <$ string "-"

op :: GenParser Char () Token
op = plus <|> minus

digitsToInt :: [Int] -> Int
digitsToInt = read . concatMap show

digit :: Int -> GenParser Char () Int
digit i = i <$ string (show i)

num :: GenParser Char () Token
num = do
  x <- many $ choice $ map digit [0..9]
  return $ TNum (digitsToInt x)

expression :: GenParser Char () Token
expression = do
  l <- num
  o <- op
  r <- num
  return $ TExpr l o r


main :: IO ()
main = do
  putStrLn ">"
  input <- getLine
  parseTest expression input
  -- let result = parse parser "" input
  -- putStrLn $ case result of
  --   Left err -> "fail"
  --   Right _ -> "ok"
