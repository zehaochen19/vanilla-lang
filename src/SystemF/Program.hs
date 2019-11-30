{-# LANGUAGE OverloadedStrings #-}

module SystemF.Program
  ( step,
    eval,
    typecheck,
    id',
    id'',
    idIdAnno,
    idUnit,
    idUnit',
    nestedId,
    nestedIdId,
    nestedIdId',
    nestedIdUnit,
    nestedIdIdUnit,
    letIdUnit,
    letNestedIdId,
    letNestedIdUnit,
    illtypedLetNestedUnit,
    unitId,
    letNestIdUnitId,
    illtypedLetNestedIdUnitIdId,
    lambdaIdIdUnit,
    applyToUnit,
    applyToUnitId,
    cont,
    runCont,
    polyLet,
  )
where

import Dynamic.Step
  ( eval,
    step,
  )
import Static.TypeCheck (typecheck)
import Syntax.Expr
import Syntax.Type

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
nestedId =
  ELam "f" (ELam "x" (EVar "f" $$ EVar "x"))
    -: TAll "B" (TAll "A" (TVar "A" --> TVar "A") --> (TVar "B" --> TVar "B"))

nestedIdId :: Expr
nestedIdId = nestedId $$ id' -: TAll "A" (TVar "A" --> TVar "A")

nestedIdUnit :: Expr
nestedIdUnit = nestedId $$ id' $$ EUnit

nestedIdId' :: Expr
nestedIdId' = (nestedId $$ id' $$ id') -: TAll "A" (TVar "A" --> TVar "A")

nestedIdIdUnit :: Expr
nestedIdIdUnit = nestedId $$ id' $$ id'' $$ EUnit

letIdUnit :: Expr
letIdUnit = ELet "myid" id' (EVar "myid" $$ EUnit)

letNestedIdId :: Expr
letNestedIdId = ELet "myNestedId" nestedId (EVar "myNestedId" $$ id')

letNestedIdUnit :: Expr
letNestedIdUnit =
  ELet "myNestedId" nestedId $
    ELet "myid" id' (EVar "myNestedId" $$ EVar "myid" $$ EUnit)

illtypedLetNestedUnit :: Expr
illtypedLetNestedUnit =
  ELet "nestedId" nestedId (EVar "nestedId" $$ EUnit $$ EUnit)

unitId = ELam "x" (EVar "x") -: TArr TUnit TUnit

letNestIdUnitId =
  ELet "nestedId" nestedId
    $ ELet "unitId" unitId
    $ EVar "nestedId"
      $$ EVar "unitId"
      $$ EUnit

illtypedLetNestedIdUnitIdId =
  ELet "nestedId" nestedId
    $ ELet "unitId" unitId
    $ EVar "nestedId"
      $$ EVar "unitId"
      $$ id'

lambdaIdIdUnit = ELam "f" (ELam "x" $ EVar "f" $$ EVar "x") $$ id' $$ EUnit

applyToUnit =
  ELam "f" (EVar "f" $$ EUnit) -: TAll "A" (TVar "A" --> TVar "A") --> TUnit

applyToUnitId = applyToUnit $$ id'

-- The continuation monad
cont :: Expr
cont =
  ELam "a" (ELam "callback" (EVar "callback" $$ EVar "a"))
    -: TAll "A" (TVar "A" --> TAll "R" ((TVar "A" --> TVar "R") --> TVar "R"))

runCont :: Expr
runCont =
  ELam "f" (ELet "callback" (ELam "x" (EVar "x")) (EVar "f" $$ EVar "callback"))
    -: TAll "A" (TAll "R" ((TVar "A" --> TVar "R") --> TVar "R") --> TVar "A")

-- polymorphic let
polyLet =
  ELet "id" id'
    $ ELet "myId" (EVar "id" $$ id')
    $ ELet "myUnit" (EVar "id" $$ EUnit)
    $ EVar "myId"
      $$ EVar "myUnit"
