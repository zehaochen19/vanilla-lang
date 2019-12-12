{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module SystemF where

import           Data.Text                      ( Text )
import           Dynamic.Step                   ( eval )
import           Parser                         ( runProgramP )
import           Polysemy
import           Polysemy.Error
import           Polysemy.State
import           Static.Context                 ( applyCtx )
import           Static.TypeCheck
import           Syntax.Expr
import           Syntax.Type

-- | Given an expression, first typecheck it and then evaluate it
interpretF :: Member (Error String) r => Expr -> Sem r (Expr, Type)
interpretF expr = do
  (ty, ctx) <-
    evalState initCheckState
    $       synthesize mempty expr
    `catch` (\e -> throw $ "Typecheck error:\n" ++ e)
  return (eval expr, applyCtx ctx ty)

systemF :: Member (Error String) r => FilePath -> Text -> Sem r (Expr, Type)
systemF src prog = do
  expr <-
    fromEither (runProgramP src prog)
      `catch` (\e -> throw $ "Parse error:\n" ++ e)
  interpretF expr

runSystemF :: FilePath -> Text -> Either String (Expr, Type)
runSystemF path src = run . runError $ systemF path src
