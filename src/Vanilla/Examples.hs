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
      Constructor "Cons" [TVar "A", TData "List" [TVar "A"]]
    ]

-- | Nat declaration
natDec =
  Declaration
    "Nat"
    []
    [Constructor "Zero" [], Constructor "Succ" [TData "Nat" []]]

unitDec = Declaration "Unit" [] [Constructor "Unit" []]

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

illtypedLetNestedUnit :: Expr
illtypedLetNestedUnit =
  ELet "nestedId" nestedId (EVar "nestedId" $$ EUnit $$ EUnit)

unitId = ELam "x" (EVar "x") -: TData "Unit" [] --> TData "Unit" []

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

polyLetNat =
  ELet "id" id'
    $ ELet "myId" (EVar "id" $$ id'')
    $ ELet "my0" (EVar "id" $$ EZero)
    $ ESucc (EVar "myId" $$ EVar "my0")

annotedIdSZero :: Expr
annotedIdSZero =
  EALam "f" (TNat --> TNat) (ESucc (EVar "f" $$ EZero))
    $$ ELam "x" (EVar "x")
    -: TNat

ifElseIdNat :: Expr
ifElseIdNat =
  EIf ETrue (ELam "x" $ EVar "x") (EALam "x" TNat $ EVar "x") -: TNat --> TNat

ifElseIdNatZero :: Expr
ifElseIdNatZero = ifElseIdNat $$ EZero

nonZero :: Expr
nonZero =
  EALam "n" TNat (ENatCase (EVar "n") ETrue "n'" EFalse) -: (TNat --> TBool)

nonZeroZero = nonZero $$ EZero

nonZeroTwo = nonZero $$ ESucc (ESucc EZero)

natAdd :: Expr
natAdd =
  EFix
    ( ELam
        "f"
        ( EALam
            "x"
            TNat
            ( EALam
                "y"
                TNat
                ( ENatCase
                    (EVar "x")
                    (EVar "y")
                    "x'"
                    (ESucc (EVar "f" $$ EVar "x'" $$ EVar "y"))
                )
            )
        )
    )

natAddAnno = natAdd -: TNat --> TNat --> TNat

natPred :: Expr
natPred =
  EALam "n" TNat (ENatCase (EVar "n") EZero "n'" (EVar "n'")) -: TNat --> TNat

natMinus :: Expr
natMinus =
  EFix
    ( ELam
        "f"
        ( EALam
            "x"
            TNat
            ( EALam
                "y"
                TNat
                ( ENatCase
                    (EVar "y")
                    (EVar "x")
                    "y'"
                    ( ENatCase
                        (EVar "x")
                        EZero
                        "x'"
                        (EVar "f" $$ EVar "x'" $$ EVar "y'")
                    )
                )
            )
        )
    )
    -: TNat
    --> TNat
    --> TNat

fibonacci :: Expr
fibonacci =
  EFix
    ( ELam
        "fib"
        ( EALam
            "n"
            TNat
            ( ENatCase
                (EVar "n")
                (ESucc EZero)
                "n'"
                ( natAdd
                    $$ (EVar "fib" $$ EVar "n'")
                    $$ (EVar "fib" $$ (natPred $$ EVar "n'"))
                )
            )
        )
    )
    -: TNat
    --> TNat

aLetId :: Expr
aLetId =
  EALet
    "id"
    (TAll "A" (TVar "A" --> TVar "A"))
    (ELam "x" $ EVar "x")
    (EVar "id" $$ ETrue)

boolNatProd :: Expr
boolNatProd = EProd ETrue (ESucc EZero)

idProd :: Expr
idProd =
  EALet
    "id"
    (TAll "A" (TVar "A" --> TVar "A"))
    (ELam "x" $ EVar "x")
    (EProd (EVar "id" $$ EFalse) (EVar "id" $$ EZero))

boolNatProj1 :: Expr
boolNatProj1 = EProj1 boolNatProd

sumUnit :: Expr
sumUnit = ELam "s" EUnit -: TAll "A" (TSum TNat (TVar "A") --> TUnit)

inj1Nat :: Expr
inj1Nat = EInj1 EZero

inj2Unit :: Expr
inj2Unit = EInj2 EUnit

isInj1 :: Expr
isInj1 =
  ELam "s" (ESumCase (EVar "s") "x" ETrue "y" EFalse)
    -: TAll "A" (TAll "B" $ TSum (TVar "A") (TVar "B") --> TBool)

listDummyProg :: Program
listDummyProg = Program [listDec] (cons "Cons" $$ EUnit $$ cons "Nil")

listEmptyProg :: Program
listEmptyProg =
  Program [listDec] $
    ECase
      (cons "Nil" -: TData "List" [TUnit])
      [Branch "Nil" [] ETrue, Branch "Cons" ["x"] EFalse]

listNonEmptyProg :: Program
listNonEmptyProg =
  Program [listDec] $
    ECase
      (cons "Cons" $$ EUnit $$ (cons "Cons" $$ EUnit $$ cons "Nil"))
      [Branch "Nil" [] ETrue, Branch "Cons" ["x"] EFalse]

nonzeroSingletonList :: Program
nonzeroSingletonList =
  Program [listDec, natDec] $
    ECase
      (cons "Succ" $$ cons "Zero")
      [ Branch "Zero" [] (cons "Nil" -@ TData "Nat" []),
        Branch "Succ" ["x"] (cons "Cons" $$ (cons "Succ" $$ EVar "x") $$ cons "Nil")
      ]

mapProgram :: Program
mapProgram =
  Program [listDec] $
    EALetRec
      "map"
      ( TAll "a" $ TAll "b" $
          (TVar "a" --> TVar "b") --> (TData "List" [TVar "a"] --> TData "List" [TVar "b"])
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
