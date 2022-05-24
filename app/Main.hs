module Main where

import Text.Parsec
import Text.Parsec.Char (alphaNum)
import Text.Parsec.String (Parser, GenParser)

data Exp = SExp String [Exp]
  | IntItem Int
  | StringItem String
  | BoolItem Bool deriving Show

whitespace :: GenParser Char () String
whitespace = string " " <|> string "\n" <|> string "\t"

stringValue :: GenParser Char () String
stringValue = many alphaNum

sexpBody :: GenParser Char () [Exp]
sexpBody = do
  option [] whitespace
  sepBy expression whitespace

sexp :: GenParser Char () Exp
sexp = do
  funcName <- stringValue
  SExp funcName <$> sexpBody

int :: GenParser Char () Int
int = read <$> many1 digit

bool :: GenParser Char () Bool
bool = True <$ string "true" <|> False <$ string "false"

expression :: GenParser Char () Exp
expression = between (string "(") (string ")") sexp
  <|> IntItem <$> int
  <|> BoolItem <$> bool
  <|> StringItem <$> stringValue

prompt :: String -> IO String
prompt promptText = do
  putStrLn promptText
  getLine

loop :: IO () -> IO ()
loop action = do
  action
  loop action

main :: IO ()
main = do
  loop $ do
    input <- prompt "Enter a LISP expression:"
    parseTest expression input
