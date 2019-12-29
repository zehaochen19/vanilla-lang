{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Vanilla.Static.TypeCheck.DeclCheck where

import           Control.Monad.Except           ( MonadError
                                                , runExcept
                                                )
import           Control.Monad.State            ( evalStateT )
import           Data.Foldable                  ( foldlM )
import qualified Data.Map                      as M
import           Data.Maybe                     ( isJust )
import           Vanilla.Static.Context         ( Context
                                                , CtxMember(..)
                                                , ctxCons
                                                , (|>)
                                                )
import           Vanilla.Static.TypeCheck.Internal
import           Vanilla.Static.TypeCheck.WellForm
import           Vanilla.Syntax.Cons            ( Constructor(..) )
import           Vanilla.Syntax.Decl            ( Declaration(..)
                                                , DeclarationMap
                                                , consType
                                                )

-- | check declarations that there are no duplicate data type names
-- and the output name -> declaration mapping
checkDataTypes :: MonadError StaticError m => [Declaration] -> m DeclarationMap
checkDataTypes = foldlM loop M.empty
 where
  loop m dec@(Declaration name _ _) = if name `M.member` m
    then throwTyErr $ DuplicateDataType name
    else pure $ M.insert name dec m

-- | check a declaration is well-formed in a context
-- and output typings of its data constructors
checkDecl :: TypeCheck m => Context -> Declaration -> m Context
checkDecl ctx dec = do
  let constrs = constructors dec
  foldlM (\c con -> checkConstructor c dec con) ctx constrs

checkDecls :: TypeCheck m => Context -> [Declaration] -> m Context
checkDecls = foldlM checkDecl

checkConstructor
  :: TypeCheck m => Context -> Declaration -> Constructor -> m Context
checkConstructor ctx dec cons@(Constructor consVar _) =
  if isJust $ ctxCons ctx consVar
    then throwTyErr $ DuplicateConstructor consVar
    else do
      let ty = consType dec cons
      ctx |- ty
      pure $ ctx |> CCons consVar ty

runCheckDataTypes :: [Declaration] -> Either StaticError DeclarationMap
runCheckDataTypes declarations =
  runExcept . flip evalStateT initCheckState $ checkDataTypes declarations
