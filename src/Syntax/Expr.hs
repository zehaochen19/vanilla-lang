{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Syntax.Expr
  ( EVar (..),
    Expr (..),
    evar,
    ($$),
    (-:),
    isELam,
  )
where

import Data.String (IsString)
import Data.Text (Text)
import Syntax.Type (Type)

newtype EVar = MkEVar Text deriving (Eq, Show, Ord, IsString)

evar :: Text -> EVar
evar = MkEVar

data Expr
  = EVar EVar
  | EUnit
  | ETrue
  | EFalse
  | EZero
  | ESucc Expr
  | ENatCase Expr Expr EVar Expr
  | EProd Expr Expr
  | ELam EVar Expr
  | EALam EVar Type Expr
  | EApp Expr Expr
  | EAnno Expr Type
  | ELet EVar Expr Expr
  | EALet EVar Type Expr Expr
  | EIf Expr Expr Expr
  | EFix Expr
  deriving (Eq, Show)

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
