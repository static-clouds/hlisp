module Main where

import Text.Parsec
import Text.Parsec.Char (alphaNum)
import Text.Parsec.String (Parser, GenParser)

data VariableValue = I Int | S String | B Bool deriving Show
data Exp = SExp String [Exp] | Literal VariableValue deriving Show

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

literal :: GenParser Char () VariableValue
literal = I <$> int
  <|> B <$> bool
  <|> S <$> many letter

expression :: GenParser Char () Exp
expression = between (string "(") (string ")") sexp
  <|> Literal <$> literal

apply :: String -> [VariableValue] -> VariableValue
apply "add" [I v1, I v2] = I (v1 + v2)
apply _ _ = I 0

eval :: Exp -> VariableValue
eval (Literal v) = v
eval (SExp funcName args) = apply funcName $ map eval args

loop :: IO () -> IO ()
loop action = do
  action
  loop action

main :: IO ()
main = do
  loop $ do
    putStrLn "Enter a LISP expression:"
    input <- getLine
    case parse expression "" input of
      Left pe -> print pe
      Right exp -> print $ eval exp
