{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Syntax.Expr
  ( EVar(..)
  , Expr(..)
  , evar
  , ($$)
  , (-:)
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
  deriving (Eq, Show)


infixl 1 $$
($$) :: Expr -> Expr -> Expr
($$) = EApp


infixl 2 -:
(-:) :: Expr -> Type -> Expr
(-:) = EAnno
