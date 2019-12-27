{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Vanilla where

import           Data.Either.Extra              ( mapLeft )
import           Data.Text                      ( Text )
import           Polysemy                       ( Sem
                                                , Member
                                                , run
                                                )
import           Polysemy.Error
import           Text.Megaparsec.Error          ( errorBundlePretty )
import           Vanilla.Dynamic.Eval           ( eval )
import           Vanilla.Parser                 ( runProgramP )
import           Vanilla.Static.Context         ( applyCtx )
import           Vanilla.Static.TypeCheck
import           Vanilla.Syntax.Expr
import           Vanilla.Syntax.Program
import           Vanilla.Syntax.Type

-- | Given a program, first typecheck it and then evaluate it
interpret :: Member (Error String) r => Program -> Sem r (Expr, Type)
interpret prog = do
  (ty, ctx) <- typeCheck prog `catch` (\e -> throw $ "Typecheck error:\n" ++ e)
  return (eval . mainExpr $ prog, applyCtx ctx ty)

vanilla :: Member (Error String) r => FilePath -> Text -> Sem r (Expr, Type)
vanilla src prog = do
  expr <-
    fromEither
    . mapLeft (\e -> "Parse error:\n" ++ errorBundlePretty e)
    $ runProgramP src prog
  interpret expr

runVanilla :: FilePath -> Text -> Either String (Expr, Type)
runVanilla path src = run . runError $ vanilla path src
