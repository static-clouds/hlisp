module Main where

import Control.Monad (forever)
import Text.Parsec (parse, sepBy)

import HLisp.Eval (evalExpression)
-- import HLisp.Parse (expression, expressions, TopLevelExp(Blank, Directive, TopLevelExp))
import HLisp.Tokenize (token, delimiter, tokenize)
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [inFilePath] -> do
      contents <- readFile inFilePath
      case parse tokenize "" contents of
        Left pe -> print pe
        Right exps -> do
          -- mapM_ eval' exps
          mapM_ print exps
    _ -> putStrLn "incorrect number of arguments"
  -- where
  --   eval' :: TopLevelExp  -> IO ()
  --   eval' Blank = pure ()
  --   eval' Directive = pure ()
  --   eval' (TopLevelExp exp) = print $ evalExpression exp



-- repl :: IO ()
-- repl = do
--   forever $ do
--     putStrLn "Enter a LISP expression:"
--     input <- getLine
--     case parse expression "" input of
--       Left pe -> print pe
--       Right exp -> do
--         print exp
--         print $ evalExpression exp
