module HLisp.Parse where

import Text.Parsec
import Text.Parsec.Char (alphaNum)


data VariableValue = I Int | S String | B Bool deriving (Eq, Show)
type Pos = (SourcePos, SourcePos)

data Exp = Identifier (Maybe Pos) String | SExp (Maybe Pos) [Exp] | Literal (Maybe Pos) VariableValue deriving Show
data TopLevelExp = Blank | Directive | TopLevelExp Exp deriving Show

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

sexp :: Parser Exp
sexp = do
  string "("
  sexpStartPos <- getSourcePos
  sexpBodyObj <- sexpBody
  string ")"
  sexpEndPos <- getSourcePos
  return $ SExp (Just (sexpStartPos, sexpEndPos)) sexpBodyObj

int :: Parser Int
int = read <$> many1 digit

bool :: Parser Bool
bool = True <$ string "true" <|> False <$ string "false"

stringToken :: Parser String
stringToken = between (string "\"") (string "\"") (many alphaNum)

literal :: Parser Exp
literal = do
  startPos <- getSourcePos
  value <- I <$> int <|> B <$> bool <|> S <$> stringToken
  endPos <- getSourcePos
  return $ Literal (Just (startPos, endPos)) value

directive :: Parser TopLevelExp
directive = do
  string "#lang racket"
  return Directive

identifier :: Parser Exp
identifier = do
  startPos <- getSourcePos
  v <- string "+" <|> string "-"
  endPos <- getSourcePos
  return $ Identifier (Just (startPos, endPos)) v

expression :: Parser Exp
expression = identifier <|> sexp <|> literal

blank :: Parser TopLevelExp
blank = do
  string ""
  return Blank

expressions :: Parser [TopLevelExp]
expressions = do
  sepBy topLevelExpression whitespace
  where
    topLevelExpression = choice [directive, TopLevelExp <$> expression, blank]
