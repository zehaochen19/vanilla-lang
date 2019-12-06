{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Data.Either
  ( isLeft,
  )
import Parser
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

parserSpec = typeParserSpec
