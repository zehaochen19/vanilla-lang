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
  , CheckState
  , TypeCheck
  )
where

import           Data.Text                      ( Text )
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Polysemy.State
import           Vanilla.Static.TypeCheck.StaticError
import           Vanilla.Syntax.Decl            ( DeclarationMap )
import           Vanilla.Syntax.Type            ( TEVar(..) )
import           Vanilla.Utils                  ( freshVarStream )

type TypeCheck r
  = Members '[Error StaticError, Reader DeclarationMap, State CheckState] r

newtype CheckState = CheckState {freshTypeVars :: [Text]}

initCheckState :: CheckState
initCheckState = CheckState freshVarStream

freshTEVar :: Member (State CheckState) r => Sem r TEVar
freshTEVar = do
  vars <- gets freshTypeVars
  put $ CheckState (tail vars)
  return $ MkTEVar . head $ vars
