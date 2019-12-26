{-# LANGUAGE OverloadedStrings #-}

module Vanilla.Examples where

import Vanilla.Syntax.Cons
import Vanilla.Syntax.Decl
import Vanilla.Syntax.Expr
import Vanilla.Syntax.Program
import Vanilla.Syntax.Type

-- | Data types

-- | List declaration
listDec :: Declaration
listDec =
  Declaration
    "List"
    ["A"]
    [ Constructor "Nil" [],
      Constructor "Cons" [TVar "A", tdata' "List" [TVar "A"]]
    ]

-- | Nat declaration
natDec =
  Declaration
    "Nat"
    []
    [Constructor "Zero" [], Constructor "Succ" [tdata "Nat"]]

-- | Bool declaration
boolDec =
  Declaration
    "Bool"
    []
    [Constructor "True" [], Constructor "False" []]

-- | Unit declaration
unitDec = Declaration "Unit" [] [Constructor "Unit" []]

-- | Product delaration
prodDec = Declaration "Prod" ["a", "b"] [Constructor "Prod" [TVar "a", TVar "b"]]

-- | Sum declaration
sumDec =
  Declaration
    "Sum"
    ["a", "b"]
    [Constructor "Inj1" [TVar "a"], Constructor "Inj2" [TVar "b"]]

id' :: Expr
id' = ELam "x" (EVar "x") -: TAll "A" (TVar "A" --> TVar "A")

id'' = ELam "y" (EVar "y") -: TAll "B" (TArr (TVar "B") (TVar "B"))

idUnit :: Program
idUnit = Program [unitDec] $ id' $$ cons "Unit"

idUnit' :: Program
idUnit' = Program [unitDec] $ ELam "x" (EVar "x") $$ cons "Unit"

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

nestedIdUnit :: Program
nestedIdUnit = Program [unitDec] $ nestedId $$ id' $$ cons "Unit"

nestedIdId' :: Expr
nestedIdId' = (nestedId $$ id' $$ id') -: TAll "A" (TVar "A" --> TVar "A")

nestedIdIdUnit :: Program
nestedIdIdUnit = Program [unitDec] $ nestedId $$ id' $$ id'' $$ cons "Unit"

letIdUnit :: Program
letIdUnit = Program [unitDec] $ ELet "myid" id' (EVar "myid" $$ cons "Unit")

letNestedIdId :: Expr
letNestedIdId = ELet "myNestedId" nestedId (EVar "myNestedId" $$ id')

letNestedIdUnit :: Program
letNestedIdUnit =
  Program [unitDec]
    $ ELet "myNestedId" nestedId
    $ ELet "myid" id' (EVar "myNestedId" $$ EVar "myid" $$ cons "Unit")

illtypedLetNestedUnit :: Program
illtypedLetNestedUnit =
  Program [unitDec] $
    ELet
      "nestedId"
      nestedId
      (EVar "nestedId" $$ cons "Unit" $$ cons "Unit")

unitId = ELam "x" (EVar "x") -: tdata "Unit" --> tdata "Unit"

illtypedLetNestIdUnitId =
  Program [unitDec]
    $ ELet "nestedId" nestedId
    $ ELet "unitId" unitId
    $ EVar "nestedId"
      $$ EVar "unitId"
      $$ cons "Unit"

illtypedLetNestedIdUnitIdId =
  Program [unitDec]
    $ ELet "nestedId" nestedId
    $ ELet "unitId" unitId
    $ EVar "nestedId"
      $$ EVar "unitId"
      $$ id'

lambdaIdIdUnit =
  Program [unitDec] $
    ELam "f" (ELam "x" $ EVar "f" $$ EVar "x") $$ id' $$ cons "Unit"

applyToUnit =
  ELam "f" (EVar "f" $$ cons "Unit") -: TAll "A" (TVar "A" --> TVar "A") --> tdata "Unit"

applyToUnitId = Program [unitDec] $ applyToUnit $$ id'

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
polyLet :: Program
polyLet =
  Program [unitDec]
    $ ELet "id" id'
    $ ELet "myId" (EVar "id" $$ id')
    $ ELet "myUnit" (EVar "id" $$ cons "Unit")
    $ EVar "myId"
      $$ EVar "myUnit"

polyLetNat :: Program
polyLetNat =
  Program [natDec]
    $ ELet "id" id'
    $ ELet "myId" (EVar "id" $$ id'')
    $ ELet "my0" (EVar "id" $$ cons "Zero")
    $ cons "Succ" $$ (EVar "myId" $$ EVar "my0")

annotedIdSZero :: Program
annotedIdSZero =
  Program [natDec] $
    EALam "f" (tdata "Nat" --> tdata "Nat") (cons "Succ" $$ (EVar "f" $$ cons "Zero"))
      $$ ELam "x" (EVar "x")
      -: tdata "Nat"

if' :: Expr
if' =
  ELam
    "b"
    ( ELam "e1" $ ELam "e2" $
        ECase
          (EVar "b")
          [Branch "True" [] $ EVar "e1", Branch "False" [] $ EVar "e2"]
    )
    -: TAll "a" (tdata "Bool" --> TVar "a" --> TVar "a" --> TVar "a")

ifElseIdNat :: Expr
ifElseIdNat =
  ELet "if" if' $
    EVar "if" $$ cons "True" $$ ELam "x" (EVar "x")
      $$ EALam "x" (tdata "Nat") (EVar "x")

ifElseIdNatZero :: Program
ifElseIdNatZero = Program [boolDec, natDec] $ ifElseIdNat $$ cons "Zero"

nonZero :: Expr
nonZero =
  EALam "n" (tdata "Nat") $
    ECase (EVar "n") [Branch "Zero" [] $ cons "False", Branch "Succ" ["n2"] $ cons "True"]

nonZeroZero = Program [natDec, boolDec] $ nonZero $$ cons "Zero"

nonZeroTwo = Program [natDec, boolDec] $ nonZero $$ (cons "Succ" $$(cons "Succ" $$ cons "Zero"))

natAdd :: Expr
natAdd =
  EFix
    ( ELam
        "f"
        ( EALam
            "x"
            (tdata "Nat")
            ( EALam
                "y"
                (tdata "Nat")
                ( ECase
                    (EVar "x")
                    [ Branch "Zero" [] $ EVar "y",
                      Branch "Succ" ["x1"] $
                        cons "Succ" $$ (EVar "f" $$ EVar "x1" $$ EVar "y")
                    ]
                )
            )
        )
    )

natAddAnno = natAdd -: tdata "Nat" --> tdata "Nat" --> tdata "Nat"

natPred :: Expr
natPred =
  EALam
    "n"
    (tdata "Nat")
    (ECase (EVar "n") [Branch "Zero" [] $ cons "Zero", Branch "Succ" ["x"] $ EVar "x"])
    -: tdata "Nat" --> tdata "Nat"

natMinus :: Expr
natMinus =
  EFix
    ( ELam
        "f"
        ( EALam
            "x"
            (tdata "Nat")
            ( EALam
                "y"
                (tdata "Nat")
                ( ECase
                    (EVar "y")
                    [ Branch "Zero" [] $ EVar "x",
                      Branch "Succ" ["y1"] $
                        ECase
                          (EVar "x")
                          [ Branch "Zero" [] $ cons "Zero",
                            Branch "Succ" ["x1"] $ EVar "f" $$ EVar "x1" $$ EVar "y1"
                          ]
                    ]
                )
            )
        )
    )
    -: tdata "Nat" --> tdata "Nat" --> tdata "Nat"

fibonacci :: Expr
fibonacci =
  EFix
    ( ELam
        "fib"
        ( EALam
            "n"
            (tdata "Nat")
            ( ECase
                (EVar "n")
                [ Branch "Zero" [] $ cons "Succ" $$ cons "Zero",
                  Branch "Succ" ["n1"] $
                    natAdd $$ (EVar "fib" $$ EVar "n1") $$ (EVar "fib" $$ (natPred $$ EVar "n1"))
                ]
            )
        )
    )
    -: tdata "Nat"
    --> tdata "Nat"

aLetId :: Program
aLetId =
  Program [boolDec] $
    EALet
      "id"
      (TAll "A" (TVar "A" --> TVar "A"))
      (ELam "x" $ EVar "x")
      (EVar "id" $$ cons "True")

boolNatProd :: Expr
boolNatProd =
  cons "Prod" $$ cons "True" $$ (cons "Succ" $$ cons "Zero")

idProd :: Program
idProd =
  Program
    [prodDec, boolDec, natDec]
    $ EALet
      "id"
      (TAll "A" (TVar "A" --> TVar "A"))
      (ELam "x" $ EVar "x")
      (cons "Prod" $$ (EVar "id" $$ cons "False") $$ (EVar "id" $$ cons "Zero"))

boolNatProj1 :: Program
boolNatProj1 =
  Program [boolDec, prodDec, natDec] $
    ECase boolNatProd [Branch "Prod" ["x", "y"] $ EVar "x"]

sumUnit :: Expr
sumUnit =
  ELam "s" (cons "Unit")
    -: TAll "A" (tdata' "Sum" [tdata "Nat", TVar "A"] --> tdata' "Unit" [])

inj1Nat :: Expr
inj1Nat = cons "Inj1" $$ cons "Zero"

inj2Unit :: Expr
inj2Unit = cons "Inj2" $$ cons "Unit"

isInj1 :: Expr
isInj1 =
  ELam "s" (ECase (EVar "s") [Branch "Inj1" ["x"] $ cons "True", Branch "Inj2" ["y"] $ cons "False"])
    -: TAll "A" (TAll "B" $ tdata' "Sum" [TVar "A", TVar "B"] --> tdata "Bool")

listDummyProg :: Program
listDummyProg = Program [listDec, unitDec] (cons "Cons" $$ cons "Unit" $$ cons "Nil")

listEmptyProg :: Program
listEmptyProg =
  Program [listDec, boolDec, unitDec] $
    ECase
      (cons "Nil" -: tdata' "List" [tdata "Unit"])
      [Branch "Nil" [] $ cons "True", Branch "Cons" ["x"] $ cons "False"]

listNonEmptyProg :: Program
listNonEmptyProg =
  Program [listDec, boolDec, unitDec] $
    ECase
      (cons "Cons" $$ cons "Unit" $$ (cons "Cons" $$ cons "Unit" $$ cons "Nil"))
      [Branch "Nil" [] $ cons "True", Branch "Cons" ["x"] $ cons "False"]

nonzeroSingletonList :: Program
nonzeroSingletonList =
  Program [listDec, natDec] $
    ECase
      (cons "Succ" $$ cons "Zero")
      [ Branch "Zero" [] (cons "Nil" -@ tdata "Nat"),
        Branch "Succ" ["x"] (cons "Cons" $$ (cons "Succ" $$ EVar "x") $$ cons "Nil")
      ]

mapProgram :: Program
mapProgram =
  Program [listDec] $
    EALetRec
      "map"
      ( TAll "a" $ TAll "b" $
          (TVar "a" --> TVar "b") --> (tdata' "List" [TVar "a"] --> tdata' "List" [TVar "b"])
      )
      ( ELam "f"
          $ ELam "xs"
          $ ECase
            (EVar "xs")
            [ Branch "Nil" [] (ETApp (cons "Nil") (TVar "b")),
              Branch
                "Cons"
                ["y", "ys"]
                (cons "Cons" $$ (EVar "f" $$ EVar "y") $$ (EVar "map" $$ EVar "f" $$ EVar "ys"))
            ]
      )
      (EVar "map")
