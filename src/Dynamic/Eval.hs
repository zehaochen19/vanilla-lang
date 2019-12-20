module Dynamic.Eval where

import Dynamic.Step
import Syntax.Expr
import Syntax.Program

eval :: Expr -> Expr
eval e = let e' = step e in if e' == e then e else eval e'

eval' :: Program -> Expr
eval' = eval . mainExpr
