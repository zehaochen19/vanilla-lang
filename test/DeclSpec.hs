{-# LANGUAGE OverloadedStrings #-}

module DeclSpec where

import Data.Map as M
import Data.Sequence as S
import Static.Context
import Syntax.Decl
import Syntax.Type
import SystemF.Examples
import Test.Hspec

declSpec = declMapSpec >> initDeclCtxSpec

declMapSpec =
  describe "declMap" $ do
    it "map List declaration to a map" $
      M.lookup "List" (declMap [listDec]) `shouldBe` Just listDec
    it "map Nat declaration to a map" $
      M.lookup "Nat" (declMap [natDec]) `shouldBe` Just natDec

initDeclCtxSpec =
  describe "initDeclCtx" $ do
    it "add List constructors into typing context" $
      initDeclCtx [listDec]
        `shouldBe` Context
          ( S.fromList
              [ CAssump "Nil" (TAll "A" (TData "List" [TVar "A"])),
                CAssump "Cons" (TAll "A" (TVar "A" --> TData "List" [TVar "A"] --> TData "List" [TVar "A"]))
              ]
          )
    it
      "add Nat constuctors into typing context"
      $ initDeclCtx [natDec]
        `shouldBe` Context
          ( S.fromList
              [ CAssump "Zero" (TData "Nat" []),
                CAssump "Succ" (TData "Nat" [] --> TData "Nat" [])
              ]
          )
