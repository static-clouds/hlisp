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

-- The following parser rules are adapted from r5rs

-- 7.1.1. Lexical structure

-- <identifier> -> <initial> <subsequent>* | <peculiar identifier>
identifier :: Parser String
identifier =  do {i <- initial; body <- many subsequent; return (i:body)} <|> peculiarIdentifier

-- <initial> -> <letter> | <special initial>
initial :: Parser Char
initial = letter' <|> specialInitial

-- <letter> -> a | b | c ... | z
letter' :: Parser Char
letter' = oneOf "abcdefghijklmnopqrstuvwxyz"

-- <special initial> -> etc
specialInitial :: Parser Char
specialInitial = oneOf  "!$%&*/:<=>?^_~"

-- <subsequent> -> <initial> | <digit> | <special subsequent>
subsequent :: Parser Char
subsequent = initial <|> digit <|> specialSubsequent

-- <digit> -> ... (built in)

-- <special subsequent> -> + | - | . | @
specialSubsequent :: Parser Char
specialSubsequent = oneOf "+-.@"

-- <peculiar identifier> -> + | - | ...
peculiarIdentifier :: Parser String
peculiarIdentifier = choice $ map string ["+", "-", "..."]

-- <syntactic keyword> -> <expression keyword> | else | => | etc
syntacticKeywordStrings :: [[Char]]
syntacticKeywordStrings = ["else", "=>", "define", "unquote", "unquote-splicing"]
syntacticKeyword :: Parser String
syntacticKeyword = choice $ map string (syntacticKeywordStrings ++ expressionKeywordStrings)

-- <expression keyword> -> quote | lambda | if | etc
expressionKeywordStrings :: [[Char]]
expressionKeywordStrings = [ "quote", "lambda", "if", "set!",
  "begin", "cond", "and", "or", "case", "let", "let*", "letrec", "do",
  "delay", "quasiquote"]
expressionKeyword :: Parser String
expressionKeyword = choice $ map string expressionKeywordStrings

-- <variable> -> any <identifier> that isn't also a <syntactic keyword>
variable :: Parser String
variable = try $ do
  value <- identifier
  if value `elem` (syntacticKeywordStrings ++ expressionKeywordStrings)
    then fail "the variable value is a syntactic keyword"
    else return value

-- <boolean> -> #t | #f
boolean :: Parser String
boolean = string "#t" <|> string "#f"

-- <character> -> #\ <any character> | #\ <character name>
character :: Parser String
character = do
  h <- string "#\\"
  t <- characterName <|> (: []) <$> anyChar
  return $ h ++ t

-- <character name> -> space | newline
characterName :: Parser String
characterName = string "space" <|> string "newline"

identifierExp :: Parser Exp
identifierExp = do
  startPos <- getSourcePos
  value <- identifier
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
