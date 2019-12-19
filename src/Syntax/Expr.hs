{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Syntax.Expr
  ( EVar (..),
    Expr (..),
    evar,
    ($$),
    (-:),
    (-@),
    isELam,
    cons,
    cons',
    Branch (..),
  )
where

import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Sequence (Seq, fromList)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Syntax.Cons (ConsVar (..))
import Syntax.Type (Type, tyParen)

newtype EVar = MkEVar Text deriving (Eq, Ord, IsString)

instance Show EVar where
  show (MkEVar v) = T.unpack v

evar :: Text -> EVar
evar = MkEVar

data Branch = Branch ConsVar [EVar] Expr deriving (Eq)

instance Show Branch where
  show (Branch cons vars e) =
    show cons ++ " " ++ unwords (show <$> vars) ++ " -> " ++ eParen e

data Expr
  = EVar EVar
  | EUnit
  | ETrue
  | EFalse
  | EZero
  | ESucc Expr
  | ENatCase Expr Expr EVar Expr
  | EProd Expr Expr
  | EProj1 Expr
  | EProj2 Expr
  | EInj1 Expr
  | EInj2 Expr
  | ESumCase Expr EVar Expr EVar Expr
  | ELam EVar Expr
  | EALam EVar Type Expr
  | EApp Expr Expr
  | EAnno Expr Type
  | ELet EVar Expr Expr
  | EALet EVar Type Expr Expr
  | EALetRec EVar Type Expr Expr
  | EIf Expr Expr Expr
  | EFix Expr
  | ECons ConsVar (Seq Expr)
  | ECase Expr [Branch]
  | ETApp Expr Type
  deriving (Eq)

isELam :: Expr -> Bool
isELam (ELam _ _) = True
isELam EALam {} = True
isELam _ = False

infixl 2 $$

($$) :: Expr -> Expr -> Expr
($$) = EApp

infixl 3 -@

(-@) :: Expr -> Type -> Expr
(-@) = ETApp

infixl 1 -:

(-:) :: Expr -> Type -> Expr
(-:) = EAnno

cons :: Text -> Expr
cons name = ECons (MkConsVar name) mempty

cons' :: Text -> [Expr] -> Expr
cons' name pat = ECons (MkConsVar name) (fromList pat)

instance Show Expr where
  show EUnit = "()"
  show (EVar v) = show v
  show (ECons name pat) =
    let patStr = if null pat then "" else " " ++ (unwords . toList . fmap eParen $ pat)
     in show name ++ patStr
  show (ECase e branch) =
    "case " ++ eParen e ++ " { "
      ++ intercalate
        ", "
        (show <$> branch)
      ++ " }"
  show ETrue = "True"
  show EFalse = "False"
  show (ESucc n) = "S " ++ eParen n
  show EZero = "0"
  show (ENatCase n e1 x e2) =
    "natcase "
      ++ eParen n
      ++ " { 0 -> "
      ++ eParen e1
      ++ ", S "
      ++ show x
      ++ " -> "
      ++ eParen e2
      ++ " }"
  show (EProd e1 e2) = "(" ++ eParen e1 ++ ", " ++ eParen e2 ++ ")"
  show (EProj1 e) = eParen e ++ ".1"
  show (EProj2 e) = eParen e ++ ".2"
  show (EInj1 e) = "Inj1 " ++ eParen e
  show (EInj2 e) = "Inj2 " ++ eParen e
  show (ESumCase e x e1 y e2) =
    "sumcase "
      ++ eParen e
      ++ " { Inj1 "
      ++ show x
      ++ " -> "
      ++ eParen e1
      ++ " , Inj2 "
      ++ show y
      ++ " -> "
      ++ eParen e2
      ++ " }"
  show (ELam x e) = "λ" ++ show x ++ " . " ++ eParen e
  show (EALam x ty e) = "λ" ++ show x ++ " : " ++ show ty ++ " . " ++ eParen e
  show (EApp e1 e2) = eParen e1 ++ " " ++ eParen e2
  show (EAnno e ty) = eParen e ++ " : " ++ tyParen ty
  show (ETApp e ty) = eParen e ++ " @" ++ tyParen ty
  show (ELet x e1 e2) =
    "let " ++ show x ++ " = " ++ eParen e1 ++ " in " ++ eParen e2
  show (EALet x ty e1 e2) =
    "let " ++ annottatedLet x ty e1 e2
  show (EALetRec x ty e1 e2) =
    "let rec " ++ annottatedLet x ty e1 e2
  show (EIf b e1 e2) =
    "if " ++ eParen b ++ " then " ++ eParen e1 ++ " else " ++ show e2
  show (EFix e) = "fix " ++ eParen e

annottatedLet x ty e1 e2 =
  show x
    ++ " : "
    ++ show ty
    ++ " = "
    ++ eParen e1
    ++ " in "
    ++ eParen e2

eParen :: Expr -> String
eParen e = case e of
  EUnit -> show e
  EVar _ -> show e
  ETrue -> show e
  EFalse -> show e
  EZero -> show e
  EProj1 _ -> show e
  EProj2 _ -> show e
  _ -> "(" ++ show e ++ ")"
