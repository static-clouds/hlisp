module HLisp.StdLib where

import HLisp.Error (EvalError(ArgumentError))
import HLisp.Parse (VariableValue(I), Exp(Literal))
-- implementations of "builtin" functions
-- i'm not trying to adhere to any standard but eventually i would like to cover r7rs or something
-- at the moment this stuff exists so that i have something to test

data Function = Func String Int ([Exp] -> Either EvalError Exp)

add :: [Exp] -> Either EvalError Exp
add [Literal _ (I i1), Literal _ (I i2)] = Right . Literal Nothing $ I $ i1 + i2
add _            = Left ArgumentError

minus :: [Exp] -> Either EvalError Exp
minus [Literal _ (I i1), Literal _ (I i2)] = Right . Literal Nothing $ I $ i1 - i2
minus _            = Left ArgumentError

functions :: [Function]
functions = [ Func "+" 2 add, Func "-" 2 minus ]
