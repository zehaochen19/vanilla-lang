{-# LANGUAGE OverloadedStrings #-}

module DeclSpec where

import           Data.Map                      as M
import           Data.Sequence                 as S
import           Test.Hspec
import           Vanilla.Examples
import           Vanilla.Static.Context
import           Vanilla.Syntax.Decl
import           Vanilla.Syntax.Type

declSpec = declMapSpec >> initDeclCtxSpec

declMapSpec = describe "declMap" $ do
  it "map List declaration to a map"
    $          M.lookup "List" (declMap [listDec])
    `shouldBe` Just listDec
  it "map Nat declaration to a map"
    $          M.lookup "Nat" (declMap [natDec])
    `shouldBe` Just natDec

initDeclCtxSpec = describe "initDeclCtx" $ do
  it "add List constructors into typing context"
    $          initDeclCtx [listDec]
    `shouldBe` Context
                 (S.fromList
                   [ CCons "Nil" (TAll "A" (tdata' "List" [TVar "A"]))
                   , CCons
                     "Cons"
                     (TAll
                       "A"
                       (TVar "A" --> tdata' "List" [TVar "A"] --> tdata'
                         "List"
                         [TVar "A"]
                       )
                     )
                   ]
                 )
  it "add Nat constuctors into typing context"
    $          initDeclCtx [natDec]
    `shouldBe` Context
                 (S.fromList
                   [ CCons "Zero" (tdata "Nat")
                   , CCons "Succ" (tdata "Nat" --> tdata "Nat")
                   ]
                 )
