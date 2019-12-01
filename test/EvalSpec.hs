{-# LANGUAGE OverloadedStrings #-}

module EvalSpec where

import Dynamic.Step (eval)
import Syntax.Expr
import SystemF.Program
import Test.Hspec

evalSpec = describe "eval" $ do
  it "evals id'" $ do
    let EAnno e _ = id'
    eval id' `shouldSatisfy` isELam
    eval id' `shouldBe` e
  it "evals id''" $ do
    let EAnno e _ = id''
    eval id'' `shouldSatisfy` isELam
    eval id'' `shouldBe` e
  it "evals idUnit" $ eval idUnit `shouldBe` EUnit
  it "evals idUnit'" $ eval idUnit' `shouldBe` EUnit
  it "evalsidIdAnno" $ do
    let res = eval idIdAnno
    print res
    res `shouldSatisfy` isELam
  it "evals nestedId" $ do
    let res = eval nestedId
    print res
    res `shouldSatisfy` isELam
  it "evals nestedIdId" $ do
    let res = eval nestedIdId
    print res
    res `shouldSatisfy` isELam
  it "evals nestedIdUnit" $ eval nestedIdUnit `shouldBe` EUnit
  it "evlas nestedIdIdUnit" $ eval nestedIdIdUnit `shouldBe` EUnit
  it "evals letIdUnit" $ eval letIdUnit `shouldBe` EUnit
  it "evals letNestedIdUnit" $ eval letNestedIdUnit `shouldBe` EUnit
  it "evals letNestIdUnitId" $ eval letNestIdUnitId `shouldBe` EUnit
  it "evals applyToUnitId" $ eval applyToUnitId `shouldBe` EUnit
  it "evals polyLetNat" $ eval polyLetNat `shouldBe` ESucc EZero
