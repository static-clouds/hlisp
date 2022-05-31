module HLisp.Error where

import HLisp.Parse (Pos)

data EvalError = EvalError Pos String | ArgumentError deriving Show
