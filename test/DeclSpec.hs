{-# LANGUAGE OverloadedStrings #-}

module DeclSpec where

import           Data.Map                      as M
import           Data.Either                    ( isLeft )
import           Test.Hspec
import           Vanilla.Examples
import           Vanilla.Static.TypeCheck.DeclCheck



declSpec = describe "declMap should" $ do
  it "map List declaration to a map" $ do
    let Right declMap = runCheckDataTypes [listDec]
    M.lookup "List" declMap `shouldBe` Just listDec
  it "map Nat declaration to a map" $ do
    let Right declMap = runCheckDataTypes [natDec]
    M.lookup "Nat" declMap `shouldBe` Just natDec
  it "reject duplicate declarations"
    $               runCheckDataTypes [natDec, natDec]
    `shouldSatisfy` isLeft


