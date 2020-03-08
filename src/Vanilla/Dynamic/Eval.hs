module Vanilla.Dynamic.Eval where

import Vanilla.Dynamic.Step
import Vanilla.Syntax.Expr
import Vanilla.Syntax.Program

eval :: Expr -> Expr
eval e = let e' = step e in if e' == e then e else eval e'

eval' :: Program -> Expr
eval' = eval . mainExpr
