{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Data.Either
  ( isLeft,
    isRight,
  )
import Parser
import Syntax.Expr
import Syntax.Type
import Test.Hspec
import Text.Megaparsec (runParser)

typeParserSpec = describe "typeP should" $ do
  it "parse Nat to Nat" $
    runParser typeP "" "Nat → Nat" `shouldBe` Right (TNat --> TNat)
  it "parse type of id" $
    runParser typeP "" "∀A.A→A" `shouldBe` Right (TAll "A" (TVar "A" --> TVar "A"))
  it "reject forall without a dot" $
    runParser typeP "" "∀A A→A" `shouldSatisfy` isLeft
  it "parse type of cont" $
    runParser typeP "" "∀A . A → ∀R.((A → R) → R)"
      `shouldBe` Right
        (TAll "A" (TVar "A" --> TAll "R" ((TVar "A" --> TVar "R") --> TVar "R")))
  it "parse type of runCont" $
    runParser typeP "" "∀A . (∀R.(A → R) → R) → A"
      `shouldBe` Right
        (TAll "A" (TAll "R" ((TVar "A" --> TVar "R") --> TVar "R") --> TVar "A"))
  it "parse type of nestedId" $
    runParser typeP "" "∀B . (∀A . A → A) → (B → B)"
      `shouldBe` Right
        (TAll "B" (TAll "A" (TVar "A" --> TVar "A") --> (TVar "B" --> TVar "B")))

expressionParseSpec =
  describe "exprP should" $ do
    it "parse nat id" $
      runParser exprP "" "λx : Nat. x"
        `shouldBe` Right (EALam "x" TNat $ EVar "x")
    it "parse annotated nat id" $
      runParser exprP "" "(λx : Nat. x) : Nat → Nat"
        `shouldBe` Right (EAnno (EALam "x" TNat $ EVar "x") (TNat --> TNat))
    it "parse unannotated id" $
      runParser exprP "" "λx . x"
        `shouldBe` Right (ELam "x" $ EVar "x")
    it "parse annoated id" $
      runParser exprP "" "(λx . x) : ∀A.A→A"
        `shouldBe` Right
          (EAnno (ELam "x" $ EVar "x") (TAll "A" (TVar "A" --> TVar "A")))
    it "parse unit with parenthesis" $
      runParser exprP "" "((()))"
        `shouldBe` Right EUnit
    it "parse application" $
      runParser exprP "" "f x" `shouldBe` Right (EApp (EVar "f") (EVar "x"))
    it "parse chain of applications" $
      runParser exprP "" "f g x" `shouldBe` Right (EVar "f" $$ EVar "g" $$ EVar "x")
    it "parse nat 1" $
      runParser exprP "" "S 0" `shouldBe` Right (ESucc EZero)
    it "parse a if-else clause" $
      runParser exprP "" "if True then 0 else S 0"
        `shouldBe` Right
          (EIf ETrue EZero (ESucc EZero))
    it "parse a let binding" $
      runParser exprP "" "let f = λx. x in f ()"
        `shouldBe` Right
          (ELet "f" (ELam "x" $ EVar "x") (EVar "f" $$ EUnit))
    it "parse a natcase with 0" $
      runParser exprP "" "natcase 0 { 0 → True, S x → False}"
        `shouldBe` Right
          (ENatCase EZero ETrue "x" EFalse)
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

parserSpec = do
  typeParserSpec
  expressionParseSpec
