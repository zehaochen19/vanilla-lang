{-# LANGUAGE OverloadedStrings #-}

module Example where

import           Static.Expr                    ( Expr(..) )
import           Static.Type                    ( Type(..) )




id' :: Expr
id' = EAnno (ELam "x" (EVar "x")) (TAll "A" (TArr (TVar "A") (TVar "A")))

idUnit :: Expr
idUnit = EApp id' EUnit

