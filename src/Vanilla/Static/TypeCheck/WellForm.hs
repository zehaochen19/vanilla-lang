module Vanilla.Static.TypeCheck.WellForm where

import           Control.Monad                  ( forM_ )
import           Data.Map                      as M
import           Data.Maybe                     ( isJust )
import           Polysemy
import           Polysemy.State                 ( gets )
import           Vanilla.Static.Context
import           Vanilla.Static.TypeCheck.Internal
import           Vanilla.Syntax.Decl
import           Vanilla.Syntax.Type

typeWellForm :: TypeCheck r => Context -> Type -> Sem r ()
typeWellForm ctx ty = case ty of
  TVar alpha | CVar alpha `ctxElem` ctx -> pure ()
  TArr a     b                          -> ctx |- a >> ctx |- b
  TAll alpha a                          -> typeWellForm (ctx |> CVar alpha) a
  TEVar ea | CEVar ea `ctxElem` ctx || isJust (ctxSolve ctx ea) -> pure ()
  TData name pat -> gets declMap >>= \decls -> case M.lookup name decls of
    Nothing  -> throwTyErr $ IllformedError ty
    Just dec -> if length pat == length (tvars dec)
      then forM_ pat (typeWellForm ctx)
      else throwTyErr $ IllformedError ty
  _ -> throwTyErr $ IllformedError ty

(|-) :: TypeCheck r => Context -> Type -> Sem r ()
(|-) = typeWellForm
