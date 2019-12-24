module Vanilla.Dynamic.Step where

import Data.Foldable (toList)
import Data.Sequence ((|>))
import Vanilla.Syntax.Expr

value :: Expr -> Bool
value (EVar _) = True
value (ECons _ _) = True
value (ELam _ _) = True
value EALam {} = True
value _ = False

-- | [x := expr1] expr2 or [expr1/x]expr2
substitute :: EVar -> Expr -> Expr -> Expr
substitute x e1 e2 =
  let loop = substitute x e1
   in case e2 of
        EVar y -> if x == y then e1 else EVar y
        ECons cName pat -> ECons cName (loop <$> pat)
        ECase e branch ->
          ECase
            (loop e)
            ( ( \(Branch cons vars e) ->
                  if x `elem` vars
                    then Branch cons vars e
                    else Branch cons vars $ loop e
              )
                <$> branch
            )
        abs@(ELam y e2') -> if x == y then abs else ELam y $ loop e2'
        abs@(EALam y ty e2') -> if x == y then abs else EALam y ty $ loop e2'
        EApp e21 e22 -> EApp (loop e21) (loop e22)
        EAnno e2' ty -> EAnno (loop e2') ty
        ETApp e2' ty -> ETApp (loop e2') ty
        ELet y e1' e2' -> ELet y (loop e1') (if x == y then e2' else loop e2')
        EALet y ty e1' e2' ->
          EALet y ty (loop e1') (if x == y then e2' else loop e2')
        EALetRec y ty e1' e2' ->
          EALetRec y ty (if x == y then e1 else loop e1') (if x == y then e2' else loop e2')
        EIf b e1' e2' -> EIf (loop b) (loop e1') (loop e2')
        EFix e -> EFix $ loop e

step :: Expr -> Expr
step expr = case expr of
  -- Lam
  abs@(ELam _ _) -> abs
  abs@EALam {} -> abs
  EApp e1 e2 | not $ value e1 -> EApp (step e1) e2
  EApp e1 e2 | not $ value e2 -> EApp e1 (step e2)
  EApp (ELam x e1) e2 -> substitute x e2 e1
  EApp (EALam x _ e1) e2 -> substitute x e2 e1
  -- Data types
  EApp (ECons name pat) e -> ECons name (pat |> e)
  ECase e branch | not . value $ e -> ECase (step e) branch
  ECase (ECons name pat) branches -> loop branches
    where
      loop [] = expr -- stuck
      loop (Branch name' vars e : _)
        | name == name' =
          foldl (\e2 (x, e1) -> substitute x e1 e2) e (zip vars $ toList pat)
      loop (_ : bs) = loop bs
  -- Anno
  EAnno e _ -> e
  -- Type application
  ETApp e _ -> e
  -- Let
  ELet x e1 e2 -> substitute x e1 e2
  EALet x ty e1 e2 -> ELet x e1 e2
  EALetRec x ty e1 e2 -> substitute x (EFix $ EALam x ty e1) e2
  -- Fixpoint
  EFix e | not $ value e -> EFix $ step e
  EFix (ELam f e) -> substitute f (EFix $ ELam f e) e
  EFix (EALam f ty e) -> substitute f (EFix $ EALam f ty e) e
  e -> e
