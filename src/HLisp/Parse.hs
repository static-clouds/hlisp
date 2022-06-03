module HLisp.Parse where

import Text.Parsec hiding (letter)
import Text.Parsec.Char (alphaNum)

import HLisp.Tokenize hiding (Parser)

data VariableValue = I Int | S String | B Bool deriving (Eq, Show)
type Pos = (SourcePos, SourcePos)

data Exp = Identifier (Maybe Pos) String | SExp (Maybe Pos) [Exp] | Literal (Maybe Pos) VariableValue deriving Show
data TopLevelExp = Blank | Directive | TopLevelExp Exp deriving Show

type Parser = Parsec String ()


getSourcePos :: Monad m => ParsecT s u m SourcePos
getSourcePos = fmap statePos getParserState

sexpBody :: Parser [Exp]
sexpBody = do
  option [] $ many whitespace
  sepBy expression $ many whitespace

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

literal :: Parser Exp
literal = do
  startPos <- getSourcePos
  value <- I <$> int <|> B <$> bool -- <|> S <$> string'
  endPos <- getSourcePos
  return $ Literal (Just (startPos, endPos)) value

directive :: Parser TopLevelExp
directive = do
  string "#lang racket"
  return Directive


identifierExp :: Parser Exp
identifierExp = do
  startPos <- getSourcePos
  (TIdentifier value) <- identifier
  endPos <- getSourcePos
  return $ Identifier (Just (startPos, endPos)) value

expression :: Parser Exp
expression = identifierExp <|> sexp <|> literal

blank :: Parser TopLevelExp
blank = do
  string ""
  return Blank

expressions :: Parser [TopLevelExp]
expressions = do
  sepBy topLevelExpression whitespace
  where
    topLevelExpression = choice [directive, TopLevelExp <$> expression, blank]
