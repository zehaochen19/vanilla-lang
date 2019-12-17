{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Syntax.Expr
  ( EVar (..),
    Expr (..),
    evar,
    ($$),
    (-:),
    isELam,
    cons,
    ConsVar (..),
  )
where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Syntax.Type (Type)

newtype EVar = MkEVar Text deriving (Eq, Ord, IsString)

instance Show EVar where
  show (MkEVar v) = T.unpack v

evar :: Text -> EVar
evar = MkEVar

newtype ConsVar = MkConsVar Text deriving (Eq, Ord, IsString)

instance Show ConsVar where
  show (MkConsVar v) = T.unpack v

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
  | EIf Expr Expr Expr
  | EFix Expr
  | ECons ConsVar (Seq Expr)
  -- TODO: ECase Expr [(EVar, Expr)]
  deriving (Eq)

isELam :: Expr -> Bool
isELam (ELam _ _) = True
isELam EALam {} = True
isELam _ = False

infixl 2 $$

($$) :: Expr -> Expr -> Expr
($$) = EApp

infixl 1 -:

(-:) :: Expr -> Type -> Expr
(-:) = EAnno

cons :: Text -> Expr
cons name = ECons (MkConsVar name) mempty

instance Show Expr where
  show EUnit = "()"
  show (EVar v) = show v
  show (ECons name pat) = show name ++ " " ++ (unwords . toList . fmap show $ pat)
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
  show (EAnno e ty) = eParen e ++ " : " ++ show ty
  show (ELet x e1 e2) =
    "let " ++ show x ++ " = " ++ eParen e1 ++ " in " ++ eParen e2
  show (EALet x ty e1 e2) =
    "let "
      ++ show x
      ++ " : "
      ++ show ty
      ++ " = "
      ++ eParen e1
      ++ " in "
      ++ eParen e2
  show (EIf b e1 e2) =
    "if " ++ eParen b ++ " then " ++ eParen e1 ++ " else " ++ show e2
  show (EFix e) = "fix " ++ eParen e

eParen :: Expr -> String
eParen e = case e of
  EUnit -> show e
  EVar _ -> show e
  ETrue -> show e
  EFalse -> show e
  EZero -> show e
  EProj1 _ -> show e
  EProj2 _ -> show e
  ECons _ _ -> show e
  _ -> "(" ++ show e ++ ")"
