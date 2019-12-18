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
import Static.Context (applyCtx)
import Static.TypeCheck
import Syntax.Expr
import Syntax.Program
import Syntax.Type
import Text.Megaparsec.Error (errorBundlePretty)

-- | Given a program, first typecheck it and then evaluate it
interpretF :: Member (Error String) r => Program -> Sem r (Expr, Type)
interpretF prog = do
  (ty, ctx) <- typecheckProg prog `catch` (\e -> throw $ "Typecheck error:\n" ++ e)
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
