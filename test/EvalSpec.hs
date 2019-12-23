{-# LANGUAGE OverloadedStrings #-}

module EvalSpec where

import Test.Hspec
import Vanilla.Dynamic.Eval (eval, eval')
import Vanilla.Examples
import Vanilla.Syntax.Expr
import Vanilla.Syntax.Program
import Vanilla.Utils
  ( intToNat',
    natToInt',
  )

evalSpec = describe "eval" $ do
  it "evals id'" $ do
    let EAnno e _ = id'
    eval id' `shouldSatisfy` isELam
    eval id' `shouldBe` e
  it "evals id''" $ do
    let EAnno e _ = id''
    eval id'' `shouldSatisfy` isELam
    eval id'' `shouldBe` e
  it "evals idUnit" $ eval' idUnit `shouldBe` cons "Unit"
  it "evals idUnit'" $ eval' idUnit' `shouldBe` cons "Unit"
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
  it "evals nestedIdUnit" $ eval' nestedIdUnit `shouldBe` cons "Unit"
  it "evlas nestedIdIdUnit" $ eval' nestedIdIdUnit `shouldBe` cons "Unit"
  it "evals letIdUnit" $ eval' letIdUnit `shouldBe` cons "Unit"
  it "evals letNestedIdUnit" $ eval' letNestedIdUnit `shouldBe` cons "Unit"
  it "evals applyToUnitId" $ eval' applyToUnitId `shouldBe` cons "Unit"
  it "evals polyLet" $ eval' polyLet `shouldBe` cons "Unit"
  it "evals polyLetNat" $ eval' polyLetNat `shouldBe` cons' "Succ" [cons "Zero"]
  it "evals annotedIdSZero" $ eval' annotedIdSZero `shouldBe` cons' "Succ" [cons "Zero"]
  it "evals ifElseIdNat" $ eval ifElseIdNat `shouldSatisfy` isELam
  it "evals ifElseIdNatZero" $ eval' ifElseIdNatZero `shouldBe` cons "Zero"
  it "evals nonZeroZero" $ eval' nonZeroZero `shouldBe` cons "False"
  it "evals nonZeroTwo" $ eval' nonZeroTwo `shouldBe` cons "True"
  it "evals 0 + 2 = 2" $
    eval (natAdd $$ intToNat' 0 $$ intToNat' 2)
      `shouldBe` intToNat' 2
  it "evals 4 + 6 = 10" $
    eval (natAdd $$ intToNat' 4 $$ intToNat' 6)
      `shouldBe` intToNat' 10
  it "evals 6 - 2 = 4" $
    eval (natMinus $$ intToNat' 6 $$ intToNat' 2)
      `shouldBe` intToNat' 4
  it "evals fibinacci 10 = 55" $
    (natToInt' . eval $ (fibonacci $$ intToNat' 10)) `shouldBe` 144
  it "evals aLetId" $ eval' aLetId `shouldBe` cons "True"
  it "evals idProd" $ eval idProd `shouldBe` EProd EFalse EZero
  it "evals boolNatProj1" $ eval' boolNatProj1 `shouldBe` cons "True"
  it "evals (isInj1 inj2Unit)" $ eval (isInj1 $$ inj2Unit) `shouldBe` EFalse
  it "evals (isInj1 inj1Nat)" $ eval (isInj1 $$ inj1Nat) `shouldBe` ETrue
  it "evals listEmptyProg" $ eval (mainExpr listEmptyProg) `shouldBe` ETrue
  it "evals listNonEmptyProg" $ eval (mainExpr listNonEmptyProg) `shouldBe` EFalse
  it "evals nonzeroSingletonList" $
    eval (mainExpr nonzeroSingletonList)
      `shouldBe` cons' "Cons" [cons' "Succ" [cons "Zero"], cons "Nil"]
