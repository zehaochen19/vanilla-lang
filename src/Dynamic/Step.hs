module Dynamic.Step where

import Syntax.Expr

value :: Expr -> Bool
value EUnit = True
value ETrue = True
value EFalse = True
value (ELam _ _) = True
value _ = False

-- | [x := expr1] expr2 or [expr1/x]expr2
substitute :: EVar -> Expr -> Expr -> Expr
substitute x e1 e2 = case e2 of
  EVar y -> if x == y then e1 else EVar y
  EUnit -> EUnit
  ETrue -> ETrue
  EFalse -> EFalse
  abs@(ELam y e2') -> if x == y then abs else ELam y $ substitute x e1 e2'
  EApp e21 e22 -> EApp (substitute x e1 e21) (substitute x e1 e22)
  EAnno e2' _ -> substitute x e1 e2'
  ELet y e1' e2' ->
    ELet y (substitute x e1 e1') (if x == y then e2' else substitute x e1 e2')

eval :: Expr -> Expr
eval e = let e' = step e in if e' == e then e else eval e'

step :: Expr -> Expr
step expr = case expr of
  EUnit -> EUnit
  ETrue -> ETrue
  EFalse -> EFalse
  abs@(ELam _ _) -> abs
  EApp e1 e2 | not $ value e1 -> EApp (step e1) e2
  EApp e1 e2 | not $ value e2 -> EApp e1 (step e2)
  EApp (ELam x e1) e2 -> substitute x e2 e1
  EAnno e _ -> e
  ELet x e1 e2 -> substitute x e1 e2
  e -> e
