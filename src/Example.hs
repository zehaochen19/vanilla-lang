{-# LANGUAGE OverloadedStrings #-}

module Example where

import           Static.Expr                    ( Expr(..) )
import           Static.Type                    ( Type(..) )




id' :: Expr
id' = EAnno (ELam "x" (EVar "x")) (TAll "A" (TArr (TVar "A") (TVar "A")))

id'' = EAnno (ELam "y" (EVar "y")) (TAll "B" (TArr (TVar "B") (TVar "B")))

idUnit :: Expr
idUnit = EApp id' EUnit

idUnit' :: Expr
idUnit' = EApp (ELam "x" (EVar "x")) EUnit

idId :: Expr
idId = EApp id' id''

idIdAnno :: Expr
idIdAnno = EAnno idId (TAll "C" (TArr (TVar "C") (TVar "C")))


nestedId :: Expr
nestedId = EAnno
  (ELam "f" (ELam "x" (EApp (EVar "f") (EVar "x"))))
  (TAll
    "A"
    (TArr (TAll "A" (TArr (TVar "A") (TVar "A"))) (TArr (TVar "A") (TVar "A")))
  )

nestedIdUnit :: Expr
nestedIdUnit = EApp (EApp nestedId id') EUnit

nestedIdId :: Expr
nestedIdId = EApp (EApp nestedId id') id'

nestedIdIdUnit :: Expr
nestedIdIdUnit = EApp (EApp (EApp nestedId id') id') EUnit
