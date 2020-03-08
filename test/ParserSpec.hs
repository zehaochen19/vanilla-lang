{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Data.Either
  ( isLeft,
    isRight,
  )
import Test.Hspec
import Text.Megaparsec (runParser)
import Vanilla.Parser
import Vanilla.Syntax.Cons
import Vanilla.Syntax.Decl
import Vanilla.Syntax.Expr
import Vanilla.Syntax.Program (Program (..))
import Vanilla.Syntax.Type

parserSpec = do
  typeParserSpec
  expressionParseSpec
  constructorPSpec
  declPSpec
  branchPSpec
  programPSpec

programPSpec =
  describe "programP should"
    $ it "parse a program with List type"
    $ runParser
      programP
      ""
      " data List a = Nil | Cons a (List a) .\
      \ data MyUnit = MyUnit .\
      \ Cons MyUnit (Cons MyUnit Nil)"
      `shouldBe` Right
        ( Program
            [ Declaration
                "List"
                ["a"]
                [ Constructor "Nil" [],
                  Constructor "Cons" [TVar "a", tdata' "List" [TVar "a"]]
                ],
              Declaration "MyUnit" [] [Constructor "MyUnit" []]
            ]
            ( cons "Cons"
                $$ cons "MyUnit"
                $$ (cons "Cons" $$ cons "MyUnit" $$ cons "Nil")
            )
        )

branchPSpec = describe "branchP should" $ do
  it "parse a Zero branch" $
    runParser branchP "" "Zero → Unit"
      `shouldBe` Right
        (Branch "Zero" [] $ cons "Unit")
  it "parse a Succ branch" $
    runParser branchP "" "Succ x → Cons x Nil"
      `shouldBe` Right
        (Branch "Succ" ["x"] (cons "Cons" $$ EVar "x" $$ cons "Nil"))

constructorPSpec = describe "constructorP should" $ do
  it "parse Nil constructor" $
    runParser constructorP "" "Nil"
      `shouldBe` Right
        (Constructor "Nil" [])
  it "parse Cons constructor" $
    runParser constructorP "" "Cons a (List a)"
      `shouldBe` Right (Constructor "Cons" [TVar "a", tdata' "List" [TVar "a"]])

declPSpec = describe "declP should" $ do
  it "parse Nat declaration" $
    runParser declP "" "data TNat = Zero | Succ TNat ."
      `shouldBe` Right
        ( Declaration
            "TNat"
            []
            [Constructor "Zero" [], Constructor "Succ" [tdata "TNat"]]
        )
  it "parse List declaration" $
    runParser declP "" "data List a = Nil | Cons a (List a) ."
      `shouldBe` Right
        ( Declaration
            "List"
            ["a"]
            [ Constructor "Nil" [],
              Constructor "Cons" [TVar "a", tdata' "List" [TVar "a"]]
            ]
        )

typeParserSpec = describe "typeP should" $ do
  it "parse Nat to Nat" $
    runParser typeP "" "Nat → Nat"
      `shouldBe` Right
        (tdata "Nat" --> tdata "Nat")
  it "parse type of id" $
    runParser typeP "" "∀a.a→a"
      `shouldBe` Right
        (TAll "a" (TVar "a" --> TVar "a"))
  it "reject forall without a dot" $
    runParser typeP "" "∀A A→A"
      `shouldSatisfy` isLeft
  it "parse type of cont" $
    runParser typeP "" "∀a . a → ∀r.((a → r) → r)"
      `shouldBe` Right
        ( TAll
            "a"
            ( TVar "a" --> TAll "r" ((TVar "a" --> TVar "r") --> TVar "r")
            )
        )
  it "parse type of runCont" $
    runParser typeP "" "∀a . (∀r.(a → r) → r) → a"
      `shouldBe` Right
        ( TAll
            "a"
            ( TAll "r" ((TVar "a" --> TVar "r") --> TVar "r") --> TVar "a"
            )
        )
  it "parse type of nestedId" $
    runParser typeP "" "∀b . (∀a . a → a) → (b → b)"
      `shouldBe` Right
        ( TAll
            "b"
            ( TAll "a" (TVar "a" --> TVar "a") --> (TVar "b" --> TVar "b")
            )
        )
  it "parse type of prod" $
    runParser typeP "" "Prod (Nat) (Nat → Nat)"
      `shouldBe` Right (tdata' "Prod" [tdata "Nat", tdata "Nat" --> tdata "Nat"])
  it "parse a pair of arrow" $
    runParser typeP "" "Prod (Nat → Nat) (Nat → Nat)"
      `shouldBe` Right
        ( tdata'
            "Prod"
            [tdata "Nat" --> tdata "Nat", tdata "Nat" --> tdata "Nat"]
        )
  it "parse a List type" $
    runParser typeP "" "List a"
      `shouldBe` Right
        (tdata' "List" [TVar "a"])
  it "parse map type" $
    runParser typeP "" "∀a. ∀b. (a → b) → (List a) → (List b)"
      `shouldBe` Right
        ( TAll "a"
            $ TAll "b"
            $ (TVar "a" --> TVar "b")
              --> (tdata' "List" [TVar "a"] --> tdata' "List" [TVar "b"])
        )

expressionParseSpec = describe "exprP should" $ do
  it "parse nat id" $
    runParser exprP "" "λx : Nat. x"
      `shouldBe` Right
        (EALam "x" (tdata "Nat") $ EVar "x")
  it "parse annotated nat id" $
    runParser exprP "" "(λx : Nat. x) : Nat → Nat"
      `shouldBe` Right
        ( EAnno
            (EALam "x" (tdata "Nat") $ EVar "x")
            (tdata "Nat" --> tdata "Nat")
        )
  it "parse unannotated id" $
    runParser exprP "" "λx . x"
      `shouldBe` Right
        (ELam "x" $ EVar "x")
  it "parse annoated id" $
    runParser exprP "" "(λx . x) : ∀a.a→a"
      `shouldBe` Right
        ( EAnno (ELam "x" $ EVar "x") (TAll "a" (TVar "a" --> TVar "a"))
        )
  it "parse unit with parenthesis" $
    runParser exprP "" "((Unit))"
      `shouldBe` Right (cons "Unit")
  it "parse application" $
    runParser exprP "" "f x"
      `shouldBe` Right
        (EApp (EVar "f") (EVar "x"))
  it "parse chain of applications" $
    runParser exprP "" "f g x"
      `shouldBe` Right
        (EVar "f" $$ EVar "g" $$ EVar "x")
  it "parse nat 1" $
    runParser exprP "" "Succ Zero"
      `shouldBe` Right
        (cons "Succ" $$ cons "Zero")
  it "parse a if-else clause" $
    runParser exprP "" "if True Zero (Succ Zero)"
      `shouldBe` Right
        ( EVar "if"
            $$ cons "True"
            $$ cons "Zero"
            $$ (cons "Succ" $$ cons "Zero")
        )
  it "parse a let binding" $
    runParser exprP "" "let f = λx. x in f Unit"
      `shouldBe` Right (ELet "f" (ELam "x" $ EVar "x") (EVar "f" $$ cons "Unit"))
  it "parse a nat case with 0" $
    runParser exprP "" "case Zero of { Zero → True, Succ x → False}"
      `shouldBe` Right
        ( ECase
            (cons "Zero")
            [ Branch "Zero" [] $ cons "True",
              Branch "Succ" ["x"] $ cons "False"
            ]
        )
  it "parse a let binding with nat case" $
    runParser
      exprP
      ""
      "let n = Succ Zero in case n of { Zero → False, Succ x → True } "
      `shouldBe` Right
        ( ELet
            "n"
            (cons "Succ" $$ cons "Zero")
            ( ECase
                (EVar "n")
                [ Branch "Zero" [] $ cons "False",
                  Branch "Succ" ["x"] $ cons "True"
                ]
            )
        )
  it "parse a chain of lambdas" $
    runParser exprP "" "λ f . λ x . f x"
      `shouldSatisfy` isRight
  it "parse a recursive function" $
    runParser
      exprP
      ""
      "(fix (λf. λx : Nat . λy : Nat. case x of {Zero → y, Succ a → Succ (f a y)})) : Nat → Nat → Nat"
      `shouldSatisfy` isRight
  it "parse a product" $
    runParser exprP "" "Prod True Zero"
      `shouldBe` Right
        (cons "Prod" $$ cons "True" $$ cons "Zero")
  it "parse a proj1" $
    runParser exprP "" "proj1 (Prod True Zero)"
      `shouldBe` Right
        (EVar "proj1" $$ (cons "Prod" $$ cons "True" $$ cons "Zero"))
  it "parse a chain of lets" $ do
    let res =
          runParser
            exprP
            ""
            "let evenodd =\
            \ fix (λ eo : Prod (Nat → Bool) (Nat → Bool).\
            \ let e = λ n : Nat. case n of { Zero → True, Succ x → (proj2 eo) x } in\
            \ let o = λ n : Nat. case n of { Zero → False, Succ x → (proj1 eo) x } in\
            \ (Prod e o))\
            \ in (proj1 evenodd)"
    print res
    res `shouldSatisfy` isRight
  it "parse a sum" $
    runParser exprP "" "Inj1 Unit : Sum (Unit) (Nat)"
      `shouldBe` Right
        ( cons "Inj1" $$ cons "Unit"
            -: tdata'
              "Sum"
              [tdata "Unit", tdata "Nat"]
        )
  it "parse a sum case" $
    runParser exprP "" "case (Inj1 Unit) of { Inj1 x → True, Inj2 y → False }"
      `shouldBe` Right
        ( ECase
            (cons "Inj1" $$ cons "Unit")
            [ Branch "Inj1" ["x"] $ cons "True",
              Branch "Inj2" ["y"] $ cons "False"
            ]
        )
  it "parse a type application" $
    runParser exprP "" "Nil @ Foo"
      `shouldBe` Right (cons "Nil" -@ tdata "Foo")
  it "parse a cons" $
    runParser exprP "" "Cons Unit Nil"
      `shouldBe` Right
        (cons "Cons" $$ cons "Unit" $$ cons "Nil")
  it "parse a pattern match expression" $
    runParser
      exprP
      ""
      "case (Cons Unit Nil) of {\
      \ Nil → True , \
      \ Cons x xs → False }"
      `shouldBe` Right
        ( ECase
            (cons "Cons" $$ cons "Unit" $$ cons "Nil")
            [ Branch "Nil" [] $ cons "True",
              Branch "Cons" ["x", "xs"] $ cons "False"
            ]
        )
  it "parse a add function defined by let rec" $
    runParser
      exprP
      ""
      "let rec add : Nat → Nat → Nat =\
      \ λ x . λ y . case x of { Zero → y, Succ a → Succ (add a y)}\
      \ in\
      \ add"
      `shouldBe` Right
        ( EALetRec
            "add"
            (tdata "Nat" --> tdata "Nat" --> tdata "Nat")
            ( ELam "x" $ ELam "y" $
                ECase
                  (EVar "x")
                  [ Branch "Zero" [] $ EVar "y",
                    Branch "Succ" ["a"] $
                      cons "Succ"
                        $$ (EVar "add" $$ EVar "a" $$ EVar "y")
                  ]
            )
            (EVar "add")
        )
