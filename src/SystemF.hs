{-# LANGUAGE OverloadedStrings #-}

module SystemF
  ( step
  , eval
  , typecheck
  , id'
  , id''
  , idIdAnno
  , idUnit
  , idUnit'
  , nestedId
  , nestedIdId
  , nestedIdId'
  , nestedIdUnit
  , nestedIdIdUnit
  )
where



import           Syntax.Expr
import           Syntax.Type
import           Dynamic.Step                   ( step
                                                , eval
                                                )
import           Static.TypeCheck               ( typecheck )


id' :: Expr
id' = ELam "x" (EVar "x") -: TAll "A" (TVar "A" --> TVar "A")

id'' = ELam "y" (EVar "y") -: TAll "B" (TArr (TVar "B") (TVar "B"))

idUnit :: Expr
idUnit = id' $$ EUnit

idUnit' :: Expr
idUnit' = ELam "x" (EVar "x") $$ EUnit

idId :: Expr
idId = EApp id' id''

idIdAnno :: Expr
idIdAnno = idId -: TAll "C" (TVar "C" --> TVar "C")


nestedId :: Expr
nestedId = ELam "f" (ELam "x" (EVar "f" $$ EVar "x"))
  -: TAll "B" (TAll "A" (TVar "A" --> TVar "A") --> (TVar "B" --> TVar "B"))

nestedIdId :: Expr
nestedIdId = nestedId $$ id' -: TAll "A" (TVar "A" --> TVar "A")

nestedIdUnit :: Expr
nestedIdUnit = nestedId $$ id' $$ EUnit

nestedIdId' :: Expr
nestedIdId' = (nestedId $$ id' $$ id') -: TAll "A" (TVar "A" --> TVar "A")

nestedIdIdUnit :: Expr
nestedIdIdUnit = nestedId $$ id' $$ id'' $$ EUnit
