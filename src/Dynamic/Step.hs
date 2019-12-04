module Dynamic.Step where

import Syntax.Expr

value :: Expr -> Bool
value EUnit = True
value ETrue = True
value EFalse = True
value EZero = True
value (ESucc n) = value n
value (ELam _ _) = True
value EALam {} = True
value _ = False

-- | [x := expr1] expr2 or [expr1/x]expr2
substitute :: EVar -> Expr -> Expr -> Expr
substitute x e1 e2 =
  let loop = substitute x e1
   in case e2 of
        EVar y -> if x == y then e1 else EVar y
        EUnit -> EUnit
        ETrue -> ETrue
        EFalse -> EFalse
        EZero -> EZero
        ESucc n -> ESucc $ loop n
        ENatCase n e1' y e2' ->
          ENatCase (loop n) (loop e1') y $
            if x == y then e2' else loop e2'
        abs@(ELam y e2') -> if x == y then abs else ELam y $ loop e2'
        abs@(EALam y ty e2') -> if x == y then abs else EALam y ty $ loop e2'
        EApp e21 e22 -> EApp (loop e21) (loop e22)
        EAnno e2' _ -> loop e2'
        ELet y e1' e2' ->
          ELet y (loop e1') (if x == y then e2' else loop e2')
        EIf b e1' e2' -> EIf (loop b) (loop e1') (loop e2')

eval :: Expr -> Expr
eval e = let e' = step e in if e' == e then e else eval e'

step :: Expr -> Expr
step expr = case expr of
  EUnit -> EUnit
  ETrue -> ETrue
  EFalse -> EFalse
  EZero -> EZero
  ESucc n -> ESucc $ step n
  ENatCase n e1 x e2 | not $ value n -> ENatCase (step n) e1 x e2
  ENatCase EZero e1 _ _ -> e1
  ENatCase (ESucc n') _ x e -> substitute x n' e
  abs@(ELam _ _) -> abs
  abs@EALam {} -> abs
  EApp e1 e2 | not $ value e1 -> EApp (step e1) e2
  EApp e1 e2 | not $ value e2 -> EApp e1 (step e2)
  EApp (ELam x e1) e2 -> substitute x e2 e1
  EApp (EALam x _ e1) e2 -> substitute x e2 e1
  EAnno e _ -> e
  ELet x e1 e2 -> substitute x e1 e2
  EIf ETrue e _ -> e
  EIf EFalse _ e -> e
  EIf b e1 e2 -> EIf (step b) e1 e2
  e -> e
