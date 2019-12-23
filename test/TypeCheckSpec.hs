{-# LANGUAGE OverloadedStrings #-}

module TypeCheckSpec where

import Data.Either
  ( isLeft,
    isRight,
  )
import Polysemy
import Polysemy.Error
import Test.Hspec
import Vanilla.Examples
import Vanilla.Static.Context (Context)
import Vanilla.Static.TypeCheck (typeCheck)
import Vanilla.Syntax.Expr
import Vanilla.Syntax.Program (Program (..))
import Vanilla.Syntax.Type

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
    let Right (ty, _) = runTypeCheck idUnit
    ty `shouldBe` TData "Unit" []
  it "infers idUnit'" $ do
    let Right (ty, _) = runTypeCheck idUnit'
    ty `shouldBe` TData "Unit" []
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
    let Right (ty, _) = runTypeCheck nestedIdUnit
    ty `shouldBe` TData "Unit" []
  it "infers nestedIdIdUnit" $ do
    let Right (ty, _) = runTypeCheck nestedIdIdUnit
    ty `shouldBe` TData "Unit" []
  it "infers letIdUnit" $ do
    let Right (ty, _) = runTypeCheck letIdUnit
    ty `shouldBe` TData "Unit" []
  it "infers letNestedIdUnit"
    $ checkAndShouldBe letNestedIdUnit
    $ TData "Unit" []
  it "rejects illtypedLetNestedUnit" $
    runTypeCheckExpr illtypedLetNestedUnit
      `shouldSatisfy` isLeft
  it "checks unitId" $ do
    let Right (ty, _) = runTypeCheck (Program [unitDec] unitId)
    ty `shouldBe` (TData "Unit" [] --> TData "Unit" [])
  it "infers illtypedLetNestIdUnitId" $
    runTypeCheck
      illtypedLetNestIdUnitId
      `shouldSatisfy` isLeft
  it "rejects illtypedLetNestedIdUnitIdId" $
    runTypeCheck illtypedLetNestedIdUnitIdId
      `shouldSatisfy` isLeft
  it "checks applyToUnit"
    $ checkAndShouldBe
      (Program [unitDec] applyToUnit)
    $ TAll "A" (TVar "A" --> TVar "A") --> TData "Unit" []
  it "checks applyToUnitId"
    $ checkAndShouldBe applyToUnitId
    $ TData "Unit" []
  it "checks cont" $ runTypeCheckExpr cont `shouldSatisfy` isRight
  it "checks runCont" $ runTypeCheckExpr runCont `shouldSatisfy` isRight
  it "infers polyLet"
    $ checkAndShouldBe polyLet
    $ TData "Unit" []
  it "infers polyLetNat"
    $ checkAndShouldBe polyLetNat
    $ TData "Nat" []
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
