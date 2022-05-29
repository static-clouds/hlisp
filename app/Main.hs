module Main where

import Data.Either (partitionEithers)
import Data.List (find)
import Text.Parsec (parse)

import Error (EvalError(EvalError))
import Parse (Exp(Literal, SExp), VariableValue, FuncName(FuncName), expression)
import StdLib (Function(Func), functions)
import Control.Monad (forever)


getFunction :: String -> Maybe Function
getFunction funcName = find (\(Func theFuncName _ _) -> theFuncName == funcName) functions


attempt :: Bool -> a -> Either a ()
attempt False err = Left err
attempt True _ = Right ()

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x)  = Right x
maybeToRight y Nothing   = Left y

eval :: Exp VariableValue  -> Either EvalError VariableValue
eval (Literal _ v) = Right v
eval (SExp sexpPos (FuncName funcPos funcName) args) = do
  -- try to get the function + arg count
  let notFoundErrorMsg = "function named '" ++ funcName ++ "' not found"
  (Func _ numFuncArgs func) <- maybeToRight (EvalError funcPos notFoundErrorMsg) (getFunction funcName)

  -- check number of args
  attempt (length args == numFuncArgs) $ EvalError funcPos "function given the wrong number of arguments"

  -- evaluate the arguments
  evaluatedArgs <- mapM eval args

  -- apply the function
  func evaluatedArgs


main :: IO ()
main = do
  forever $ do
    putStrLn "Enter a LISP expression:"
    input <- getLine
    case parse expression "" input of
      Left pe -> print pe
      Right exp -> do
        print exp
        print $ eval exp
