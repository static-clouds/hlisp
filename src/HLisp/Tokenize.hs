module HLisp.Tokenize where

import Data.Functor (($>))
import Text.Parsec hiding (letter, token)

type Parser = Parsec String ()

data Token = TIdentifier String
  | TBoolean Bool
  | TNumber Int
  | TCharacter String
  | TString String
  | TSymbol String
  deriving Eq

instance Show Token where
  show (TBoolean True) = "#t"
  show (TBoolean False) = "#f"
  show (TIdentifier s) = s
  show (TNumber i) = show i
  show (TCharacter c) = "#\\" ++ c
  show (TString s) = "\"" ++ s ++ "\""
  show (TSymbol s) = s

-- The following parser rules are adapted from r5rs

-- 7.1.1. Lexical structure
-- <token> -> ...
token :: Parser Token
token = identifier <|> boolean <|> number <|> character <|> string' <|> tsymbol

tsymbol :: Parser Token
tsymbol = TSymbol <$> choice (map string ["(" , ")", "#(", "'", "`", ",", ",@", "."])

-- <delimiter> -> <whitespace> | ( | ) | " | ;
delimiter :: Parser String
delimiter = many whitespace <|> choice (map string ["(", ")", "\"", ";"])

-- <whitespace> -> <space> | <newline>
whitespace :: Parser Char
whitespace = char ' ' <|> char '\n'

-- <comment> -> all subsequent characters up to a line break
comment :: Parser String
comment = do
  string ";"
  v <- many anyChar
  string "\n"
  -- between (string ";") (string "\n") (many anyChar)
  return v

-- <atmosphere> -> <whitespace> | <comment>
atmosphere :: Parser String
atmosphere = many whitespace <|> comment

-- <intertoken space> -> <atmosphere>*
intertokenSpace :: Parser String
intertokenSpace = concat <$> many atmosphere

-- <identifier> -> <initial> <subsequent>* | <peculiar identifier>
identifier :: Parser Token
identifier =  TIdentifier <$> (standardIdentifier <|> peculiarIdentifier)
  where
    standardIdentifier = do
      i <- initial
      body <- many subsequent
      return (i:body)

-- <initial> -> <letter> | <special initial>
initial :: Parser Char
initial = letter <|> specialInitial

-- <letter> -> a | b | c ... | z
letter :: Parser Char
letter = oneOf "abcdefghijklmnopqrstuvwxyz"

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
-- variable :: Parser String
-- variable = try $ do
--   value <- identifier
--   if value `elem` (syntacticKeywordStrings ++ expressionKeywordStrings)
--     then fail "the variable value is a syntactic keyword"
--     else return value

-- <boolean> -> #t | #f
boolean :: Parser Token
boolean = TBoolean <$> (string "#t" $> True <|> string "#f" $> False)

-- <character> -> #\ <any character> | #\ <character name>
character :: Parser Token
character = do
  h <- string "#\\"
  t <- characterName <|> (: []) <$> anyChar
  return $ TCharacter $ h ++ t

-- <character name> -> space | newline
characterName :: Parser String
characterName = string "space" <|> string "newline"

-- <string> -> " <string element>* "
string' :: Parser Token
string' = TString <$> between (string "\"") (string "\"") (concat <$> many stringElement)

-- <string element> -> <any char other than " or \> | \" | \\
escapedBackSlash :: String
escapedBackSlash = "\\\\"

escapedDoubleQuote :: String
escapedDoubleQuote = "\\\""

anyCharExcept :: [Char] -> Parser Char
anyCharExcept rejects = do
  c <- anyChar
  if c `elem` rejects then fail $ "c must not be one of " ++ show rejects else return c

stringElement :: Parser String
stringElement = string escapedBackSlash <|> string escapedDoubleQuote <|> (: []) <$> anyCharExcept ['\\', '\"']

data NumIndex = N2 | N8 | N10 | N16 deriving Show

-- <number> -> <num 2> | <num 8> | <num 10> | <num 16>
-- TODO: currently we are just supporting exact decimal integers, more complicated number types can come later
number :: Parser Token
number = do
  s <- sign
  value <- many1 digit
  return $ TNumber $ signToMultiplier s * read value

sign :: Parser String
sign = choice $ map string ["+", "-", ""]

signToMultiplier :: String -> Int
signToMultiplier "-" = -1
signToMultiplier _   = 1

tokenize :: Parser [Token]
tokenize = endBy token atmosphere
