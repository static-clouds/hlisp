module HLisp.StdLib where

import HLisp.Error (EvalError(ArgumentError))
import HLisp.Parse (VariableValue(I))
-- implementations of "builtin" functions
-- i'm not trying to adhere to any standard but eventually i would like to cover r7rs or something
-- at the moment this stuff exists so that i have something to test

data Function = Func String Int ([VariableValue] -> Either EvalError VariableValue)

add :: [VariableValue] -> Either EvalError VariableValue
add [I i1, I i2] = Right . I $ i1 + i2
add _            = Left ArgumentError

minus :: [VariableValue] -> Either EvalError VariableValue
minus [I i1, I i2] = Right . I $ i1 - i2
minus _            = Left ArgumentError

functions :: [Function]
functions = [ Func "add" 2 add, Func "minus" 2 minus ]
