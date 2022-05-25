module Main where

import Data.Either (partitionEithers)
import Text.Parsec
import Text.Parsec.Char (alphaNum)
import Text.Parsec.String (Parser, GenParser)
import Data.List

data Token = LParen
  | RParen
  | Whitespace
  | Symbol String deriving Show

data VariableValue = I Int | S String | B Bool deriving Show
type Pos = (SourcePos, SourcePos)
data FuncName = FuncName Pos String deriving Show
data Exp = SExp Pos FuncName [Exp] | Literal Pos VariableValue deriving Show

data EvalError = EvalError Pos String | ArgumentError deriving Show

data Function = Func String Int ([VariableValue] -> Either EvalError VariableValue)

add :: [VariableValue] -> Either EvalError VariableValue
add [I i1, I i2] = Right . I $ i1 + i2
add _            = Left ArgumentError

functions :: [Function]
functions = [ Func "add" 2 add ]

getFunction :: String -> Maybe Function
getFunction funcName = find (\(Func theFuncName _ _) -> theFuncName == funcName) functions

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


reduceOrReturn :: [Either a b] -> Either a [b]
reduceOrReturn xs = case leftValues of
  [] -> Right $ rightValues
  (x:xs) -> Left x
  where (leftValues, rightValues) = partitionEithers xs


eval :: Exp -> Either EvalError VariableValue
eval (Literal _ v) = Right v
eval (SExp sexpPos (FuncName funcPos funcName) args) = do
  -- try to get the function + arg count
  (Func _ numFuncArgs func) <- case getFunction funcName of
    Just function -> Right function
    -- if fail, return evalerror
    Nothing -> Left $ EvalError funcPos ("function named '" ++ funcName ++ "' not found")

  -- check number of args
  if length args /= numFuncArgs
    then Left $ EvalError funcPos "function given the wrong number of arguments" else Right ()

  -- evaluate the arguments
  evaluatedArgs <- reduceOrReturn $ map eval args

  -- apply the function
  func evaluatedArgs


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
