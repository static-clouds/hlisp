module HLisp.Error where

import HLisp.Parse (Pos)

data EvalError = EvalError (Maybe Pos) String | ArgumentError deriving Show
