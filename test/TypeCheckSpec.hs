{-# LANGUAGE OverloadedStrings #-}

module TypeCheckSpec where

import Data.Either
  ( isLeft,
    isRight,
  )
import Polysemy
import Polysemy.Error
import Static.Context (Context)
import Static.TypeCheck (typeCheck)
import Syntax.Expr
import Syntax.Program (Program (..))
import Syntax.Type
import Test.Hspec
import Vanilla.Examples

runTypeCheck :: Program -> Either String (Type, Context)
runTypeCheck p = run . runError $ typeCheck p

runTypeCheckExpr e = runTypeCheck $ Program [] e

resShouldBe res ty = do
  res `shouldSatisfy` isRight
  let Right (ty', _) = res
  ty' `shouldBe` ty

checkExprAndShouldBe e ty = do
  let res = runTypeCheckExpr e
  resShouldBe res ty

checkAndShouldBe p ty = do
  let res = runTypeCheck p
  resShouldBe res ty

typeCheckSpec = describe "typeCheck" $ do
  it "infers id'" $ do
    let id'Res = runTypeCheckExpr id'
    id'Res `shouldSatisfy` isRight
    let Right (ty, _) = id'Res
    ty `shouldSatisfy` isTAll
  it "infers id''" $ do
    let id''Res = runTypeCheckExpr id''
    id''Res `shouldSatisfy` isRight
    let Right (ty, _) = id''Res
    ty `shouldSatisfy` isTAll
  it "infers idUnit" $ do
    let Right (ty, _) = runTypeCheckExpr idUnit
    ty `shouldBe` TUnit
  it "infers idUnit'" $ do
    let Right (ty, _) = runTypeCheckExpr idUnit
    ty `shouldBe` TUnit
  it "infers idIdAnno" $ do
    let Right (ty, _) = runTypeCheckExpr idIdAnno
    ty `shouldBe` TAll "C" (TVar "C" --> TVar "C")
  it "infers nestedId" $ do
    let Right (ty, _) = runTypeCheckExpr nestedId
    ty
      `shouldBe` TAll
        "B"
        (TAll "A" (TVar "A" --> TVar "A") --> (TVar "B" --> TVar "B"))
  it "infers nestedIdId" $ do
    let Right (ty, _) = runTypeCheckExpr nestedIdId
    ty `shouldBe` TAll "A" (TVar "A" --> TVar "A")
  it "infers nestedIdUnit" $ do
    let Right (ty, _) = runTypeCheckExpr nestedIdUnit
    ty `shouldBe` TUnit
  it "infers nestedIdIdUnit" $ do
    let Right (ty, _) = runTypeCheckExpr nestedIdIdUnit
    ty `shouldBe` TUnit
  it "infers letIdUnit" $ do
    let Right (ty, _) = runTypeCheckExpr letIdUnit
    ty `shouldBe` TUnit
  it "infers letNestedIdUnit" $ do
    let Right (ty, _) = runTypeCheckExpr letNestedIdUnit
    ty `shouldBe` TUnit
  it "rejects illtypedLetNestedUnit" $
    runTypeCheckExpr illtypedLetNestedUnit
      `shouldSatisfy` isLeft
  it "checks unitId" $ do
    let Right (ty, _) = runTypeCheckExpr unitId
    ty `shouldBe` TArr TUnit TUnit
  it "infers letNestIdUnitId" $ do
    let Right (ty, _) = runTypeCheckExpr letNestedIdUnit
    ty `shouldBe` TUnit
  it "rejects illtypedLetNestedIdUnitIdId" $
    runTypeCheckExpr illtypedLetNestedIdUnitIdId
      `shouldSatisfy` isLeft
  it "checks applyToUnit" $
    let Right (ty, _) = runTypeCheckExpr applyToUnit
     in ty `shouldBe` TAll "A" (TVar "A" --> TVar "A") --> TUnit
  it "checks applyToUnitId" $
    let Right (ty, _) = runTypeCheckExpr applyToUnitId in ty `shouldBe` TUnit
  it "checks cont" $ runTypeCheckExpr cont `shouldSatisfy` isRight
  it "checks runCont" $ runTypeCheckExpr runCont `shouldSatisfy` isRight
  it "infers polyLet" $ do
    let Right (ty, _) = runTypeCheckExpr polyLet
    ty `shouldBe` TUnit
  it "infers polyLetNat" $ do
    let Right (ty, _) = runTypeCheckExpr polyLetNat
    ty `shouldBe` TNat
  it "infers annotedIdSZero" $ do
    let res = runTypeCheckExpr annotedIdSZero
    res `shouldSatisfy` isRight
    let Right (ty, _) = res
    ty `shouldBe` TNat
  it "checks ifElseIdNat" $ checkExprAndShouldBe ifElseIdNat $ TNat --> TNat
  it "infers ifElseIdNatZero" $ checkExprAndShouldBe ifElseIdNatZero TNat
  it "checks nonZero" $ checkExprAndShouldBe nonZero $ TNat --> TBool
  it "infers nonZeroZero" $ checkExprAndShouldBe nonZeroZero TBool
  it "infers nonZeroTwo" $ checkExprAndShouldBe nonZeroTwo TBool
  it "infers natAdd" $ checkExprAndShouldBe natAdd $ TNat --> TNat --> TNat
  it "checks natAddAnno" $ checkExprAndShouldBe natAddAnno $ TNat --> TNat --> TNat
  it "checks natMinus" $ checkExprAndShouldBe natMinus $ TNat --> TNat --> TNat
  it "checks fibonacci" $ checkExprAndShouldBe fibonacci $ TNat --> TNat
  it "infers aLetId" $ checkExprAndShouldBe aLetId TBool
  it "infers boolNatProd" $ checkExprAndShouldBe boolNatProd $ TProd TBool TNat
  it "infers idProd" $ checkExprAndShouldBe idProd $ TProd TBool TNat
  it "infers boolNatProj1" $ checkExprAndShouldBe boolNatProj1 TBool
  it "checks sumUnit" $ checkExprAndShouldBe sumUnit $
    TAll
      "A"
      (TSum TNat (TVar "A") --> TUnit)
  it "infers (sumUnit inj1Nat)" $ checkExprAndShouldBe (EApp sumUnit inj1Nat) TUnit
  it "infers (sumUnit inj2Unit" $
    checkExprAndShouldBe (EApp sumUnit inj2Unit) TUnit
  it "checks isInj1" $ runTypeCheckExpr isInj1 `shouldSatisfy` isRight
  it "infers (isInj1 inj2Unit)" $ checkExprAndShouldBe (isInj1 $$ inj2Unit) TBool
  it "infers (isInj1 inj1Nat)" $ checkExprAndShouldBe (isInj1 $$ inj1Nat) TBool
  it "checks listDummyProg"
    $ checkAndShouldBe listDummyProg
    $ TData "List" [TUnit]
  it "checks listEmptyProg" $ checkAndShouldBe listEmptyProg TBool
  it "checks listNonEmptyProg" $ checkAndShouldBe listNonEmptyProg TBool
  it "checks nonzeroSingletonList"
    $ checkAndShouldBe nonzeroSingletonList
    $ TData "List" [TData "Nat" []]
  it "infers map function" $ checkAndShouldBe mapProgram
    $ TAll "a"
    $ TAll "b"
    $ (TVar "a" --> TVar "b") --> (TData "List" [TVar "a"] --> TData "List" [TVar "b"])
