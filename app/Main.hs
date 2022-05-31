module Main where

import Control.Monad (forever)
import Text.Parsec (parse)

import HLisp.Eval (eval)
import HLisp.Parse (expression)


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
