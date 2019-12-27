{-# LANGUAGE OverloadedStrings #-}

module TypeCheckSpec where

import           Data.Either                    ( isLeft
                                                , isRight
                                                )
import           Polysemy
import           Polysemy.Error
import           Test.Hspec
import           Vanilla.Examples
import           Vanilla.Static.Context         ( Context )
import           Vanilla.Static.TypeCheck       ( typeCheck )
import           Vanilla.Syntax.Expr
import           Vanilla.Syntax.Program         ( Program(..) )
import           Vanilla.Syntax.Type

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
    ty `shouldBe` tdata "Unit"
  it "infers idUnit'" $ do
    let Right (ty, _) = runTypeCheck idUnit'
    ty `shouldBe` tdata "Unit"
  it "infers idIdAnno" $ do
    let Right (ty, _) = runTypeCheckExpr idIdAnno
    ty `shouldBe` TAll "C" (TVar "C" --> TVar "C")
  it "infers nestedId" $ do
    let Right (ty, _) = runTypeCheckExpr nestedId
    ty `shouldBe` TAll
      "B"
      (TAll "A" (TVar "A" --> TVar "A") --> (TVar "B" --> TVar "B"))
  it "infers nestedIdId" $ do
    let Right (ty, _) = runTypeCheckExpr nestedIdId
    ty `shouldBe` TAll "A" (TVar "A" --> TVar "A")
  it "infers nestedIdUnit" $ do
    let Right (ty, _) = runTypeCheck nestedIdUnit
    ty `shouldBe` tdata "Unit"
  it "infers nestedIdIdUnit" $ do
    let Right (ty, _) = runTypeCheck nestedIdIdUnit
    ty `shouldBe` tdata "Unit"
  it "infers letIdUnit" $ do
    let Right (ty, _) = runTypeCheck letIdUnit
    ty `shouldBe` tdata "Unit"
  it "infers letNestedIdUnit" $ checkAndShouldBe letNestedIdUnit $ tdata "Unit"
  it "rejects illtypedLetNestedUnit"
    $               runTypeCheck illtypedLetNestedUnit
    `shouldSatisfy` isLeft
  it "checks unitId" $ do
    let Right (ty, _) = runTypeCheck (Program [unitDec] unitId)
    ty `shouldBe` (tdata "Unit" --> tdata "Unit")
  it "infers illtypedLetNestIdUnitId"
    $               runTypeCheck illtypedLetNestIdUnitId
    `shouldSatisfy` isLeft
  it "rejects illtypedLetNestedIdUnitIdId"
    $               runTypeCheck illtypedLetNestedIdUnitIdId
    `shouldSatisfy` isLeft
  it "checks applyToUnit"
    $   checkAndShouldBe (Program [unitDec] applyToUnit)
    $   TAll "A" (TVar "A" --> TVar "A")
    --> tdata "Unit"
  it "checks applyToUnitId" $ checkAndShouldBe applyToUnitId $ tdata "Unit"
  it "checks cont" $ runTypeCheckExpr cont `shouldSatisfy` isRight
  it "checks runCont" $ runTypeCheckExpr runCont `shouldSatisfy` isRight
  it "infers polyLet" $ checkAndShouldBe polyLet $ tdata "Unit"
  it "infers polyLetNat" $ checkAndShouldBe polyLetNat $ tdata "Nat"
  it "infers annotedIdSZero" $ checkAndShouldBe annotedIdSZero $ tdata "Nat"
  it "checks if" $ checkAndShouldBe (Program [boolDec] if') $ TAll
    "a"
    (tdata "Bool" --> TVar "a" --> TVar "a" --> TVar "a")
  it "checks ifElseIdNat"
    $   checkAndShouldBe (Program [natDec, boolDec] ifElseIdNat)
    $   tdata "Nat"
    --> tdata "Nat"
  it "infers ifElseIdNatZero" $ checkAndShouldBe ifElseIdNatZero $ tdata "Nat"
  it "checks nonZero"
    $   checkAndShouldBe (Program [natDec, boolDec] nonZero)
    $   tdata "Nat"
    --> tdata "Bool"
  it "infers nonZeroZero" $ checkAndShouldBe nonZeroZero $ tdata "Bool"
  it "infers nonZeroTwo" $ checkAndShouldBe nonZeroTwo $ tdata "Bool"
  it "infers natAdd"
    $   checkAndShouldBe (Program [natDec] natAdd)
    $   tdata "Nat"
    --> tdata "Nat"
    --> tdata "Nat"
  it "checks natAddAnno"
    $   checkAndShouldBe (Program [natDec] natAddAnno)
    $   tdata "Nat"
    --> tdata "Nat"
    --> tdata "Nat"
  it "checks natMinus"
    $   checkAndShouldBe (Program [natDec] natMinus)
    $   tdata "Nat"
    --> tdata "Nat"
    --> tdata "Nat"
  it "checks fibonacci"
    $   checkAndShouldBe (Program [natDec] fibonacci)
    $   tdata "Nat"
    --> tdata "Nat"
  it "infers aLetId" $ checkAndShouldBe aLetId $ tdata "Bool"
  it "infers boolNatProd"
    $ checkAndShouldBe (Program [boolDec, prodDec, natDec] boolNatProd)
    $ tdata' "Prod" [tdata "Bool", tdata "Nat"]
  it "infers idProd" $ checkAndShouldBe idProd $ tdata'
    "Prod"
    [tdata "Bool", tdata "Nat"]
  it "infers boolNatProj1" $ checkAndShouldBe boolNatProj1 $ tdata "Bool"
  it "checks sumUnit"
    $ checkAndShouldBe (Program [unitDec, sumDec, natDec] sumUnit)
    $ TAll "A" (tdata' "Sum" [tdata "Nat", TVar "A"] --> tdata "Unit")
  it "infers (sumUnit inj1Nat)"
    $ checkAndShouldBe
        (Program [unitDec, sumDec, natDec] $ EApp sumUnit inj1Nat)
    $ tdata "Unit"
  it "infers (sumUnit inj2Unit)"
    $ checkAndShouldBe
        (Program [unitDec, sumDec, natDec] $ EApp sumUnit inj2Unit)
    $ tdata "Unit"
  it "checks isInj1"
    $               runTypeCheck (Program [sumDec, boolDec] isInj1)
    `shouldSatisfy` isRight
  it "infers (isInj1 inj2Unit)"
    $ checkAndShouldBe
        (Program [sumDec, boolDec, natDec, unitDec] $ isInj1 $$ inj2Unit)
    $ tdata "Bool"
  it "infers (isInj1 inj1Nat)"
    $ checkAndShouldBe
        (Program [sumDec, boolDec, natDec, unitDec] $ isInj1 $$ inj1Nat)
    $ tdata "Bool"
  it "checks listDummyProg" $ checkAndShouldBe listDummyProg $ tdata'
    "List"
    [tdata "Unit"]
  it "checks listEmptyProg" $ checkAndShouldBe listEmptyProg $ tdata "Bool"
  it "checks listNonEmptyProg" $ checkAndShouldBe listNonEmptyProg $ tdata
    "Bool"
  it "checks nonzeroSingletonList"
    $ checkAndShouldBe nonzeroSingletonList
    $ tdata' "List" [tdata "Nat"]
  it "infers map function"
    $   checkAndShouldBe mapProgram
    $   TAll "a"
    $   TAll "b"
    $   (TVar "a" --> TVar "b")
    --> (tdata' "List" [TVar "a"] --> tdata' "List" [TVar "b"])
