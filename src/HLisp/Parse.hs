module HLisp.Parse where

import Text.Parsec
import Text.Parsec.Char (alphaNum)


data VariableValue = I Int | S String | B Bool deriving Show
type Pos = (SourcePos, SourcePos)
data FuncName = FuncName Pos String deriving Show

data Exp = SExp Pos FuncName [Exp] | Literal Pos VariableValue deriving Show

type Parser = Parsec String ()


getSourcePos :: Monad m => ParsecT s u m SourcePos
getSourcePos = fmap statePos getParserState


whitespace :: Parser String
whitespace = string " " <|> string "\n" <|> string "\t"

stringValue :: Parser String
stringValue = do
  string "\""
  res <- many alphaNum
  string "\""
  return res

sexpBody :: Parser [Exp]
sexpBody = do
  option [] whitespace
  sepBy expression whitespace

funcName :: Parser FuncName
funcName = do
  funcNameStartPos <- getSourcePos
  funcNameText <- many alphaNum
  funcNameEndPos <- getSourcePos
  return $ FuncName (funcNameStartPos, funcNameEndPos) funcNameText

sexp :: Parser Exp
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

int :: Parser Int
int = read <$> many1 digit

bool :: Parser Bool
bool = True <$ string "true" <|> False <$ string "false"

literal :: Parser Exp
literal = do
  startPos <- getSourcePos
  value <- I <$> int <|> B <$> bool <|> S <$> many letter
  endPos <- getSourcePos
  return $ Literal (startPos, endPos) value

expression :: Parser Exp
expression = sexp <|> literal
