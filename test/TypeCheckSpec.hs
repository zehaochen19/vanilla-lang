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
  it "infers annotedIdSZero"
    $ checkAndShouldBe annotedIdSZero
    $ TData "Nat" []
  it "checks if" $ checkAndShouldBe (Program [boolDec] if') $
    TAll "a" (TData "Bool" [] --> TVar "a" --> TVar "a" --> TVar "a")
  it "checks ifElseIdNat"
    $ checkAndShouldBe
      (Program [natDec, boolDec] ifElseIdNat)
    $ TData "Nat" [] --> TData "Nat" []
  it "infers ifElseIdNatZero" $ checkAndShouldBe ifElseIdNatZero $ TData "Nat" []
  it "checks nonZero"
    $ checkAndShouldBe (Program [natDec, boolDec] nonZero)
    $ TData "Nat" [] --> TData "Bool" []
  it "infers nonZeroZero" $ checkAndShouldBe nonZeroZero $ TData "Bool" []
  it "infers nonZeroTwo" $ checkAndShouldBe nonZeroTwo $ TData "Bool" []
  it "infers natAdd" $ checkAndShouldBe (Program [natDec] natAdd) $
    TData "Nat" [] --> TData "Nat" [] --> TData "Nat" []
  it "checks natAddAnno" $ checkAndShouldBe (Program [natDec] natAddAnno) $
    TData "Nat" [] --> TData "Nat" [] --> TData "Nat" []
  it "checks natMinus" $ checkAndShouldBe (Program [natDec] natMinus) $
    TData "Nat" [] --> TData "Nat" [] --> TData "Nat" []
  it "checks fibonacci" $ checkAndShouldBe (Program [natDec] fibonacci) $
    TData "Nat" [] --> TData "Nat" []
  it "infers aLetId" $ checkAndShouldBe aLetId $ TData "Bool" []
  it "infers boolNatProd"
    $ checkAndShouldBe
      ( Program
          [boolDec, prodDec, natDec]
          boolNatProd
      )
    $ TData "Prod" [TData "Bool" [], TData "Nat" []]
  it "infers idProd" $ checkAndShouldBe idProd $ TData "Prod" [TData "Bool" [], TData "Nat" []]
  it "infers boolNatProj1" $ checkAndShouldBe boolNatProj1 $ TData "Bool" []
  it "checks sumUnit" $ checkAndShouldBe (Program [unitDec, sumDec, natDec] sumUnit) $
    TAll "A" (TData "Sum" [TData "Nat" [], TVar "A"] --> TData "Unit" [])
  it "infers (sumUnit inj1Nat)"
    $ checkAndShouldBe
      ( Program [unitDec, sumDec, natDec] $
          EApp sumUnit inj1Nat
      )
    $ TData "Unit" []
  it "infers (sumUnit inj2Unit)"
    $ checkAndShouldBe (Program [unitDec, sumDec, natDec] $ EApp sumUnit inj2Unit)
    $ TData "Unit" []
  it "checks isInj1" $ runTypeCheck (Program [sumDec, boolDec] isInj1) `shouldSatisfy` isRight
  it "infers (isInj1 inj2Unit)"
    $ checkAndShouldBe (Program [sumDec, boolDec, natDec, unitDec] $ isInj1 $$ inj2Unit)
    $ TData "Bool" []
  it "infers (isInj1 inj1Nat)"
    $ checkAndShouldBe (Program [sumDec, boolDec, natDec, unitDec] $ isInj1 $$ inj1Nat)
    $ TData "Bool" []
  it "checks listDummyProg"
    $ checkAndShouldBe listDummyProg
    $ TData "List" [TData "Unit" []]
  it "checks listEmptyProg" $ checkAndShouldBe listEmptyProg $ TData "Bool" []
  it "checks listNonEmptyProg" $ checkAndShouldBe listNonEmptyProg $ TData "Bool" []
  it "checks nonzeroSingletonList"
    $ checkAndShouldBe nonzeroSingletonList
    $ TData "List" [TData "Nat" []]
  it "infers map function" $ checkAndShouldBe mapProgram
    $ TAll "a"
    $ TAll "b"
    $ (TVar "a" --> TVar "b") --> (TData "List" [TVar "a"] --> TData "List" [TVar "b"])
