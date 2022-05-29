module Error where

import Parse (Pos)

data EvalError = EvalError Pos String | ArgumentError deriving Show
