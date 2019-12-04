{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module SystemF where

import Dynamic.Step (eval)
import Polysemy
import Polysemy.Error
import Polysemy.State
import Static.Context (applyCtx)
import Static.TypeCheck
import Syntax.Expr
import Syntax.Type

-- | Given an expression, first typecheck it and then evaluate it
interpret :: Member (Error String) r => Expr -> Sem r (Expr, Type)
interpret expr = do
  (ty, ctx) <-
    evalState initCheckState $
      synthesize mempty expr
        `catch` (\e -> throw $ "Typecheck error:\n" ++ e)
  return (eval expr, applyCtx ctx ty)
