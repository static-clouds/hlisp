module Main where

import Data.Either (partitionEithers)
import Data.List (find)
import Text.Parsec (parse)

import Error (EvalError(EvalError))
import Parse (Exp(Literal, SExp), Pos, VariableValue, FuncName(FuncName), expression)
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

foldExp :: (Pos -> VariableValue -> r) -> (Pos -> FuncName -> [r] -> r) -> Exp -> r
foldExp literal sexp tree = case tree of
  Literal literalPos vv -> literal literalPos vv
  SExp sExpPos fn exps -> sexp sExpPos fn $ map (foldExp literal sexp) exps


eval :: Exp -> Either EvalError VariableValue
eval = foldExp literal sexp
  where
    literal _ vv = Right vv
    sexp sexpPos (FuncName funcPos funcName) vars = do
      -- try to get the function + arg count
      let notFoundErrorMsg = "function named '" ++ funcName ++ "' not found"
      (Func _ numFuncArgs func) <- maybeToRight (EvalError funcPos notFoundErrorMsg) (getFunction funcName)

      -- check number of args
      attempt (length vars == numFuncArgs) $ EvalError funcPos "function given the wrong number of arguments"

      -- evaluate the arguments
      evaluatedArgs <- sequence vars

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
