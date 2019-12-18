{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Data.Either
  ( isLeft,
    isRight,
  )
import Parser
import Syntax.Cons
import Syntax.Decl
import Syntax.Expr
import Syntax.Program (Program (..))
import Syntax.Type
import Test.Hspec
import Text.Megaparsec (runParser)

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
                  Constructor "Cons" [TVar "a", TData "List" [TVar "a"]]
                ],
              Declaration
                "MyUnit"
                []
                [Constructor "MyUnit" []]
            ]
            (cons "Cons" $$ cons "MyUnit" $$ (cons "Cons" $$ cons "MyUnit" $$ cons "Nil"))
        )

branchPSpec = describe "branchP should" $ do
  it "parse a Zero branch" $
    runParser branchP "" "Zero → ()" `shouldBe` Right (Branch "Zero" [] EUnit)
  it "parse a Succ branch" $
    runParser branchP "" "Succ x → Cons x Nil"
      `shouldBe` Right (Branch "Succ" ["x"] (cons "Cons" $$ EVar "x" $$ cons "Nil"))

constructorPSpec = describe "constructorP should" $ do
  it "parse Nil constructor" $
    runParser constructorP "" "Nil" `shouldBe` Right (Constructor "Nil" [])
  it "parse Cons constructor" $
    runParser constructorP "" "Cons a (List a)"
      `shouldBe` Right (Constructor "Cons" [TVar "a", TData "List" [TVar "a"]])

declPSpec = describe "declP should" $ do
  it "parse Nat declaration" $
    runParser declP "" "data TNat = Zero | Succ TNat ."
      `shouldBe` Right
        (Declaration "TNat" [] [Constructor "Zero" [], Constructor "Succ" [TData "TNat" []]])
  it "parse List declaration" $
    runParser declP "" "data List a = Nil | Cons a (List a) ."
      `shouldBe` Right
        ( Declaration
            "List"
            ["a"]
            [ Constructor "Nil" [],
              Constructor "Cons" [TVar "a", TData "List" [TVar "a"]]
            ]
        )

typeParserSpec = describe "typeP should" $ do
  it "parse Nat to Nat" $
    runParser typeP "" "Nat → Nat"
      `shouldBe` Right
        (TNat --> TNat)
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
    runParser typeP "" "(Nat, Nat → Nat)"
      `shouldBe` Right (TProd TNat (TNat --> TNat))
  it "parse a pair of arrow" $
    runParser typeP "" "(Nat → Nat, Nat → Nat)"
      `shouldBe` Right (TProd (TNat --> TNat) (TNat --> TNat))
  it "parse a List type" $
    runParser typeP "" "List a" `shouldBe` Right (TData "List" [TVar "a"])
  it "parse map type" $
    runParser typeP "" "∀a. ∀b. (a → b) → (List a) → (List b)"
      `shouldBe` Right
        ( TAll "a" $ TAll "b" $
            (TVar "a" --> TVar "b") --> (TData "List" [TVar "a"] --> TData "List" [TVar "b"])
        )

expressionParseSpec = describe "exprP should" $ do
  it "parse nat id" $
    runParser exprP "" "λx : Nat. x"
      `shouldBe` Right
        (EALam "x" TNat $ EVar "x")
  it "parse annotated nat id" $
    runParser exprP "" "(λx : Nat. x) : Nat → Nat"
      `shouldBe` Right (EAnno (EALam "x" TNat $ EVar "x") (TNat --> TNat))
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
    runParser exprP "" "((()))"
      `shouldBe` Right EUnit
  it "parse application" $
    runParser exprP "" "f x"
      `shouldBe` Right
        (EApp (EVar "f") (EVar "x"))
  it "parse chain of applications" $
    runParser exprP "" "f g x"
      `shouldBe` Right
        (EVar "f" $$ EVar "g" $$ EVar "x")
  it "parse nat 1" $ runParser exprP "" "S 0" `shouldBe` Right (ESucc EZero)
  it "parse a if-else clause" $
    runParser exprP "" "if True then 0 else S 0"
      `shouldBe` Right (EIf ETrue EZero (ESucc EZero))
  it "parse a let binding" $
    runParser exprP "" "let f = λx. x in f ()"
      `shouldBe` Right (ELet "f" (ELam "x" $ EVar "x") (EVar "f" $$ EUnit))
  it "parse a natcase with 0" $
    runParser exprP "" "natcase 0 { 0 → True, S x → False}"
      `shouldBe` Right (ENatCase EZero ETrue "x" EFalse)
  it "parse a let binding with natcase" $
    runParser exprP "" "let n = S 0 in natcase n { 0 → False, S x → True } "
      `shouldBe` Right
        (ELet "n" (ESucc EZero) (ENatCase (EVar "n") EFalse "x" ETrue))
  it "parse a chain of lambdas" $
    runParser exprP "" "λ f . λ x . f x"
      `shouldSatisfy` isRight
  it "parse a recursive function" $
    runParser
      exprP
      ""
      "(fix (λf. λx : Nat . λy : Nat. natcase x {0 → y, S a → S (f a y)})) : Nat → Nat → Nat"
      `shouldSatisfy` isRight
  it "parse a product" $
    runParser exprP "" "(True, 0)"
      `shouldBe` Right
        (EProd ETrue EZero)
  it "parse a proj1" $
    runParser exprP "" "(True, 0).1"
      `shouldBe` Right
        (EProj1 (EProd ETrue EZero))
  it "parse a chain of lets" $ do
    let res =
          runParser
            exprP
            ""
            "let evenodd =\
            \ fix (λ eo : (Nat → Bool, Nat → Bool).\
            \ let e = λ n : Nat. natcase n { 0 → True, S x → eo.2 x } in\
            \ let o = λ n : Nat. natcase n { 0 → False, S x → eo.1 x } in\
            \ (e, o))\
            \ in evenodd.1"
    print res
    res `shouldSatisfy` isRight
  it "parse a sum" $
    runParser exprP "" "(Inj1 ()) : Unit + Nat"
      `shouldBe` Right (EInj1 EUnit -: TSum TUnit TNat)
  it "parse a sumcase" $
    runParser exprP "" "sumcase (Inj1 ()) { Inj1 x → True, Inj2 y → False }"
      `shouldBe` Right (ESumCase (EInj1 EUnit) "x" ETrue "y" EFalse)
  it "parse a type application" $
    runParser exprP "" "Nil @ Foo"
      `shouldBe` Right (cons "Nil" -@ TData "Foo" [])
  it "parse a cons" $
    runParser exprP "" "Cons () Nil" `shouldBe` Right (cons "Cons" $$ EUnit $$ cons "Nil")
  it "parse a pattern match expression" $
    runParser
      exprP
      ""
      "case (Cons () Nil) of {\
      \ Nil → True , \
      \ Cons x xs → False }"
      `shouldBe` Right
        ( ECase
            (cons "Cons" $$ EUnit $$ cons "Nil")
            [Branch "Nil" [] ETrue, Branch "Cons" ["x", "xs"] EFalse]
        )
