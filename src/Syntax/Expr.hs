{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Syntax.Expr
  ( EVar(..)
  , Expr(..)
  , evar
  , ($$)
  , (-:)
  , isELam
  )
where


import           Syntax.Type                    ( Type )
import           Data.String                    ( IsString )

newtype EVar = MkEVar String deriving(Eq, Show, Ord, IsString)


evar :: String -> EVar
evar = MkEVar

data Expr
  = EVar EVar
  | EUnit
  | ELam EVar Expr
  | EApp Expr Expr
  | EAnno Expr Type
  | ELet EVar Expr Expr
  deriving (Eq, Show)

isELam :: Expr -> Bool
isELam (ELam _ _) = True
isELam _          = False

infixl 2 $$
($$) :: Expr -> Expr -> Expr
($$) = EApp


infixl 1 -:
(-:) :: Expr -> Type -> Expr
(-:) = EAnno
