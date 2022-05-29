module Parse where

import Text.Parsec
import Text.Parsec.Char (alphaNum)


data VariableValue = I Int | S String | B Bool deriving Show
type Pos = (SourcePos, SourcePos)
data FuncName = FuncName Pos String deriving Show

data Exp = SExp Pos FuncName [Exp] | Literal Pos VariableValue deriving Show


getSourcePos :: Monad m => ParsecT s u m SourcePos
getSourcePos = fmap statePos getParserState


whitespace :: Parsec String () String
whitespace = string " " <|> string "\n" <|> string "\t"

stringValue :: Parsec String () String
stringValue = do
  string "\""
  res <- many alphaNum
  string "\""
  return res

sexpBody :: Parsec String () [Exp]
sexpBody = do
  option [] whitespace
  sepBy expression whitespace

funcName :: Parsec String () FuncName
funcName = do
  funcNameStartPos <- getSourcePos
  funcNameText <- many alphaNum
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
