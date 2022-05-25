module Main where

import Text.Parsec
import Text.Parsec.Char (alphaNum)
import Text.Parsec.String (Parser, GenParser)

data Token = LParen
  | RParen
  | Whitespace
  | Symbol String deriving Show

data VariableValue = I Int | S String | B Bool deriving Show
type Pos = (SourcePos, SourcePos)
data FuncName = FuncName Pos String deriving Show
data Exp = SExp Pos FuncName [Exp] | Literal Pos VariableValue deriving Show

getSourcePos :: Monad m => ParsecT s u m SourcePos
getSourcePos = fmap statePos getParserState


whitespace :: Parsec String () String
whitespace = string " " <|> string "\n" <|> string "\t"

stringValue :: Parsec String () String
stringValue = many alphaNum

sexpBody :: Parsec String () [Exp]
sexpBody = do
  option [] whitespace
  sepBy expression whitespace

funcName :: Parsec String () FuncName
funcName = do
  funcNameStartPos <- getSourcePos
  funcNameText <- stringValue
  funcNameEndPos <- getSourcePos
  return $ FuncName (funcNameStartPos, funcNameEndPos) funcNameText

sexp :: Parsec String () Exp
sexp = do
  string "("
  sexpStartPos <- getSourcePos
  option [] whitespace
  funcNameObj <- funcName
  option [] whitespace
  sexpBodyObj <- sexpBody
  string ")"
  sexpEndPos <- getSourcePos
  return $ SExp (sexpStartPos, sexpEndPos) funcNameObj sexpBodyObj

int :: Parsec String () Int
int = read <$> many1 digit

bool :: Parsec String () Bool
bool = True <$ string "true" <|> False <$ string "false"

literal :: Parsec String () Exp
literal = do
  startPos <- getSourcePos
  value <- I <$> int <|> B <$> bool <|> S <$> many letter
  endPos <- getSourcePos
  return $ Literal (startPos, endPos) value

expression :: Parsec String () Exp
expression = sexp <|> literal

apply :: String -> [VariableValue] -> Maybe VariableValue
apply "add" [I v1, I v2] = Just $ I (v1 + v2)
apply _ _ = Nothing

eval :: Exp -> VariableValue
eval (Literal _ v) = v
eval (SExp _ (FuncName _ funcName) args) = case apply funcName $ map eval args of
  Nothing -> I 0
  Just vv -> vv


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
      Right exp -> do
        print exp
        print $ eval exp
