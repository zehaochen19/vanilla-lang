{-# LANGUAGE OverloadedStrings #-}

module Example where

import           Static.Expr                    ( Expr(..) )
import           Static.Type                    ( Type(..) )




id' :: Expr
id' = EAnno (ELam "x" (EVar "x")) (TAll "B" (TArr (TVar "B") (TVar "B")))

idUnit :: Expr
idUnit = EApp id' EUnit

idId :: Expr
idId = EApp id' id'

idIdAnno :: Expr
idIdAnno = EAnno idId (TAll "A" (TArr (TVar "A") (TVar "A")))


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
