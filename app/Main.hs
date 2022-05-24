module Main where

import Text.Parsec
import Text.Parsec.Char (alphaNum)
import Text.Parsec.String (Parser, GenParser)

data VariableValue = I Int | S String | B Bool deriving Show
data Exp = SExp String [Exp] | Literal VariableValue deriving Show


whitespace :: Parsec String () String
whitespace = string " " <|> string "\n" <|> string "\t"

stringValue :: Parsec String () String
stringValue = many alphaNum

sexpBody :: Parsec String () [Exp]
sexpBody = do
  option [] whitespace
  sepBy expression whitespace

sexp :: Parsec String () Exp
sexp = do
  funcName <- stringValue
  SExp funcName <$> sexpBody

int :: Parsec String () Int
int = read <$> many1 digit

bool :: Parsec String () Bool
bool = True <$ string "true" <|> False <$ string "false"

literal :: Parsec String () VariableValue
literal = I <$> int
  <|> B <$> bool
  <|> S <$> many letter

expression :: Parsec String () Exp
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
