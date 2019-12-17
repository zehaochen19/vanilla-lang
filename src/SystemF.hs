{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module SystemF where

import Data.Either.Extra (mapLeft)
import Data.Text (Text)
import Dynamic.Step (eval)
import Parser (runProgramP)
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.State
import Static.Context (applyCtx)
import Static.TypeCheck
import Syntax.Decl
import Syntax.Expr
import Syntax.Program
import Syntax.Type
import Text.Megaparsec.Error (errorBundlePretty)

-- | Given an expression, first typecheck it and then evaluate it
interpretF :: Member (Error String) r => Expr -> Sem r (Expr, Type)
interpretF expr = do
  (ty, ctx) <-
    runReader emptyDecls . evalState initCheckState $
      synthesize mempty expr
        `catch` (\e -> throw $ "Typecheck error:\n" ++ e)
  return (eval expr, applyCtx ctx ty)

interpretF' :: Member (Error String) r => Program -> Sem r (Expr, Type)
interpretF' prog = do
  (ty, ctx) <- typecheck' prog `catch` (\e -> throw $ "Typecheck error:\n" ++ e)
  return (eval . mainExpr $ prog, applyCtx ctx ty)

systemF :: Member (Error String) r => FilePath -> Text -> Sem r (Expr, Type)
systemF src prog = do
  expr <-
    fromEither
      . mapLeft
        (\e -> "Parse error:\n" ++ errorBundlePretty e)
      $ runProgramP src prog
  interpretF expr

runSystemF :: FilePath -> Text -> Either String (Expr, Type)
runSystemF path src = run . runError $ systemF path src
