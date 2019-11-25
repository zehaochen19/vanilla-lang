{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Static.TypeCheck where

import           Polysemy
import           Polysemy.Error
import           Polysemy.State
import           Static.Type                    ( Type(..)
                                                , TVar
                                                , TEVar(..)
                                                , tyFreeTEVars
                                                , isMono
                                                )
import           Static.Context
import           Static.WellForm

import           Utils                          ( freshVarStream )

-- | Apply bidirectional typechecking


newtype CheckState = CheckState {
    freshTypeVars :: [String]
  }

initCheckState :: CheckState
initCheckState = CheckState freshVarStream


freshTEVar :: Member (State CheckState) r => Sem r TEVar
freshTEVar = do
  vars <- gets freshTypeVars
  put $ CheckState (tail vars)
  return $ MkTEVar . head $ vars


-- | [ty1/alpha]ty2
tySubstitue :: TVar -> Type -> Type -> Type
tySubstitue alpha ty1 ty2 = case ty2 of
  TUnit        -> TUnit
  TVar  alpha' -> if alpha == alpha' then ty1 else ty2
  TEVar _      -> ty2
  TAll beta a ->
    if alpha == beta then ty2 else TAll beta (tySubstitue alpha ty1 a)


subtype
  :: Members '[Error String, State CheckState] r
  => Context
  -> Type
  -> Type
  -> Sem r Context
-- <:Var
subtype ctx (TVar a) (TVar a') | a == a' = pure ctx
-- <:Unit
subtype ctx TUnit TUnit                  = pure ctx
-- <:ExVar
subtype ctx (TEVar alpha) (TEVar alpha') | alpha == alpha' = pure ctx
-- <:-->
subtype ctx (TArr a1 a2) (TArr b1 b2)    = do
  theta <- subtype ctx b1 a1
  subtype theta (applyCtx theta a2) (applyCtx theta b2)
-- <:forallL
subtype ctx (TAll alpha a) b = do
  alphaHat <- freshTEVar
  let ctx' = ctx |> CMarker alphaHat |> CEVar alphaHat
      a'   = tySubstitue alpha (TEVar alphaHat) a
  ctxUntil (CMarker alphaHat) <$> subtype ctx' a' b
-- <:forallR
subtype ctx a (TAll alpha b) =
  ctxUntil (CVar alpha) <$> subtype (ctx |> CVar alpha) a b
-- <:InstantiateL
subtype ctx (TEVar alphaHat) a | alphaHat `notElem` tyFreeTEVars a =
  instantiateL ctx alphaHat a
-- <:InstantiateR
subtype ctx a (TEVar alphaHat) | alphaHat `notElem` tyFreeTEVars a =
  instantiateR ctx a alphaHat
subtype _ a b =
  throw $ "cannot establish subtyping with " ++ show a ++ " " ++ show b



instantiateL
  :: Members '[Error String, State CheckState] r
  => Context
  -> TEVar
  -> Type
  -> Sem r Context
-- InstLSolve
instantiateL ctx ea ty
  |
-- TODO: add type wellformedness in premise
    isMono ty, Just (gamma, gamma') <- ctxHole (CEVar ea) ctx
  = pure $ gamma |> CSolve ea ty <> gamma'
-- InstLReach
instantiateL ctx ea (TEVar eb) = undefined


instantiateR
  :: Members '[Error String, State CheckState] r
  => Context
  -> Type
  -> TEVar
  -> Sem r Context
instantiateR = undefined
