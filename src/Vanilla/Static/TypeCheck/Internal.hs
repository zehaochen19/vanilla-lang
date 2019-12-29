{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Vanilla.Static.TypeCheck.Internal
  ( StaticError(..)
  , TypeCheckError(..)
  , throwTyErr
  , initCheckState
  , freshTEVar
  , CheckState(..)
  , TypeCheck
  )
where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Text                      ( Text )
import           Vanilla.Static.TypeCheck.StaticError
import           Vanilla.Syntax.Decl            ( DeclarationMap )
import           Vanilla.Syntax.Type            ( TEVar(..) )
import           Vanilla.Utils                  ( freshVarStream )

type TypeCheck m
  = ( MonadReader DeclarationMap m
    , MonadError StaticError m
    , MonadState CheckState m
    )

newtype CheckState
  = CheckState
      { freshTypeVars :: [Text]
      }

initCheckState :: CheckState
initCheckState = CheckState freshVarStream

freshTEVar :: TypeCheck m => m TEVar
freshTEVar = do
  vars <- gets freshTypeVars
  modify $ \s -> s { freshTypeVars = tail vars }
  return $ MkTEVar . head $ vars
