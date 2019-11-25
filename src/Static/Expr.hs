module Static.Expr where


import           Static.Type                    ( Type )

newtype EVar = MkEVar String deriving(Eq, Show)

data Expr
  = EVar EVar
  | EUnit
  | ELam EVar Expr
  | EApp Expr Expr
  | EAnno Expr Type
