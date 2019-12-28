{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Vanilla where

import Control.Monad.Except
import Data.Either.Extra (mapLeft)
import Data.Text (Text)
import Text.Megaparsec.Error (errorBundlePretty)
import Vanilla.Dynamic.Eval (eval)
import Vanilla.Parser (runProgramP)
import Vanilla.Static.Context (applyCtx)
import Vanilla.Static.TypeCheck
import Vanilla.Syntax.Expr
import Vanilla.Syntax.Program
import Vanilla.Syntax.Type

-- | Given a program, first typecheck it and then evaluate it
interpret :: MonadError String m => Program -> m (Expr, Type)
interpret prog = do
  (ty, ctx) <-
    liftEither
      $ mapLeft (\e -> "Typecheck error:\n" ++ show e)
      $ runTypeCheck prog
  return (eval . mainExpr $ prog, applyCtx ctx ty)

vanilla :: MonadError String m => FilePath -> Text -> m (Expr, Type)
vanilla src prog = do
  expr <-
    liftEither
      $ mapLeft (\e -> "Parse error:\n" ++ errorBundlePretty e)
      $ runProgramP src prog
  interpret expr

runVanilla :: FilePath -> Text -> Either String (Expr, Type)
runVanilla path src = runExcept $ vanilla path src
