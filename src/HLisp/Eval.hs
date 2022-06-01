module HLisp.Eval where

import Data.Either (partitionEithers)
import Data.List (find)

import HLisp.Error (EvalError(EvalError))
import HLisp.Parse (Exp(Identifier, Literal, SExp), Pos, VariableValue, expression)
import HLisp.StdLib (Function(Func), functions)


getFunction :: String -> Maybe Function
getFunction funcName = find (\(Func theFuncName _ _) -> theFuncName == funcName) functions


attempt :: Bool -> a -> Either a ()
attempt False err = Left err
attempt True _ = Right ()

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x)  = Right x
maybeToRight y Nothing   = Left y

foldExp :: (Maybe Pos -> String -> r) -> (Maybe Pos -> VariableValue -> r) -> (Maybe Pos -> [r] -> r) -> Exp -> r
foldExp identifier literal sexp tree = case tree of
  Identifier identifierPos identifierName -> identifier identifierPos identifierName
  Literal literalPos vv -> literal literalPos vv
  SExp sExpPos exps -> sexp sExpPos $ map (foldExp identifier literal sexp) exps


evalExpression :: Exp -> Either EvalError Exp
evalExpression = foldExp identifier literal sexp
  where
    identifier p v = Right $ Identifier p v
    literal p v = Right $ Literal p v
    sexp :: Maybe Pos -> [Either EvalError Exp] -> Either EvalError Exp
    sexp sexpPos vars = case vars of
      [] -> Left $ EvalError sexpPos "s-expressions should have at least one item"
      (x:xs) -> do
        -- evaluate the first argument
        x' <- x
        headItem <- evalExpression x'
        case headItem of
          Identifier identifierPos funcName -> do
            -- try to get the function + arg count
            let notFoundErrorMsg = "function named '" ++ funcName ++ "' not found"
            (Func _ numFuncArgs func) <- maybeToRight (EvalError identifierPos notFoundErrorMsg) (getFunction funcName)

            -- check number of args
            attempt (length xs == numFuncArgs) $ EvalError identifierPos "function given the wrong number of arguments"

            -- evaluate the rest of the arguments
            xs' <- sequence xs
            xs'' <- mapM evalExpression xs'
            func xs''
          _ -> Left $ EvalError sexpPos "The head of the s-expression must be an identifier (function?)"
