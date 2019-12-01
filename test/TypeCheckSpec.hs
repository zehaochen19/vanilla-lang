{-# LANGUAGE OverloadedStrings #-}

module TypeCheckSpec where

import Data.Either
  ( isLeft,
    isRight,
  )
import Static.TypeCheck (typecheck)
import Syntax.Type
import SystemF.Program
import Test.Hspec

typecheckSpec = describe "typeckeck" $ do
  it "infers id'" $ do
    let id'Res = typecheck id'
    id'Res `shouldSatisfy` isRight
    let Right (ty, _) = id'Res
    ty `shouldSatisfy` isTAll
  it "infers id''" $ do
    let id''Res = typecheck id''
    id''Res `shouldSatisfy` isRight
    let Right (ty, _) = id''Res
    ty `shouldSatisfy` isTAll
  it "infers idUnit" $ do
    let Right (ty, _) = typecheck idUnit
    ty `shouldBe` TUnit
  it "infers idUnit'" $ do
    let Right (ty, _) = typecheck idUnit
    ty `shouldBe` TUnit
  it "infers idIdAnno" $ do
    let Right (ty, _) = typecheck idIdAnno
    ty `shouldBe` TAll "C" (TVar "C" --> TVar "C")
  it "infers nestedId" $ do
    let Right (ty, _) = typecheck nestedId
    ty
      `shouldBe` TAll
        "B"
        (TAll "A" (TVar "A" --> TVar "A") --> (TVar "B" --> TVar "B"))
  it "infers nestedIdId" $ do
    let Right (ty, _) = typecheck nestedIdId
    ty `shouldBe` TAll "A" (TVar "A" --> TVar "A")
  it "infers nestedIdUnit" $ do
    let Right (ty, _) = typecheck nestedIdUnit
    ty `shouldBe` TUnit
  it "infers nestedIdIdUnit" $ do
    let Right (ty, _) = typecheck nestedIdIdUnit
    ty `shouldBe` TUnit
  it "infers letIdUnit" $ do
    let Right (ty, _) = typecheck letIdUnit
    ty `shouldBe` TUnit
  it "infers letNestedIdUnit" $ do
    let Right (ty, _) = typecheck letNestedIdUnit
    ty `shouldBe` TUnit
  it "rejects illtypedLetNestedUnit" $
    typecheck illtypedLetNestedUnit
      `shouldSatisfy` isLeft
  it "checks unitId" $ do
    let Right (ty, _) = typecheck unitId
    ty `shouldBe` TArr TUnit TUnit
  it "infers letNestIdUnitId" $ do
    let Right (ty, _) = typecheck letNestedIdUnit
    ty `shouldBe` TUnit
  it "rejects illtypedLetNestedIdUnitIdId" $
    typecheck illtypedLetNestedIdUnitIdId
      `shouldSatisfy` isLeft
  it "checks applyToUnit" $
    let Right (ty, _) = typecheck applyToUnit
     in ty `shouldBe` TAll "A" (TVar "A" --> TVar "A") --> TUnit
  it "checks applyToUnitId" $
    let Right (ty, _) = typecheck applyToUnitId in ty `shouldBe` TUnit
  it "checks cont" $ typecheck cont `shouldSatisfy` isRight
  it "checks runCont" $ typecheck runCont `shouldSatisfy` isRight
  it "infers polyLet" $ do
    let Right (ty, _) = typecheck polyLet
    ty `shouldBe` TUnit
  it "infers polyLetNat" $ do
    let Right (ty, _) = typecheck polyLetNat
    ty `shouldBe` TNat
