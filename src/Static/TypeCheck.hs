{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Static.TypeCheck where

import Data.Text (Text)
import Polysemy
import Polysemy.Error
import Polysemy.State
import Static.Context
import Static.WellForm
import Syntax.Expr (Expr (..))
import Syntax.Type
  ( TEVar (..),
    TVar,
    Type (..),
    isMono,
    tyFreeTEVars,
    tyFreeTVars,
  )
import Utils (freshVarStream)

-- | Apply bidirectional typechecking
newtype CheckState = CheckState {freshTypeVars :: [Text]}

initCheckState :: CheckState
initCheckState = CheckState freshVarStream

type TypeCheck r = Members '[Error String, State CheckState] r

freshTEVar :: Member (State CheckState) r => Sem r TEVar
freshTEVar = do
  vars <- gets freshTypeVars
  put $ CheckState (tail vars)
  return $ MkTEVar . head $ vars

-- | [ty1/alpha]ty2
tySubstitue :: TVar -> Type -> Type -> Type
tySubstitue alpha ty1 ty2 =
  if not . null $ tyFreeTVars ty1
    then error $ "Non-closed type: " ++ show ty1
    else case ty2 of
      TUnit -> TUnit
      TBool -> TBool
      TNat -> TNat
      TVar alpha' -> if alpha == alpha' then ty1 else ty2
      TEVar _ -> ty2
      TAll beta a ->
        if alpha == beta then ty2 else TAll beta (tySubstitue alpha ty1 a)
      TArr a b -> TArr (tySubstitue alpha ty1 a) (tySubstitue alpha ty1 b)
      TProd a b -> TProd (tySubstitue alpha ty1 a) (tySubstitue alpha ty1 b)

subtype :: TypeCheck r => Context -> Type -> Type -> Sem r Context
-- <:Var
subtype ctx (TVar a) (TVar a') | a == a' = pure ctx
-- <:Unit
subtype ctx TUnit TUnit = pure ctx
-- <:Bool
subtype ctx TBool TBool = pure ctx
-- <:Nat
subtype ctx TNat TNat = pure ctx
-- <:ExVar
subtype ctx (TEVar alpha) (TEVar alpha') | alpha == alpha' = pure ctx
-- <:Product
subtype ctx (TProd a1 b1) (TProd a2 b2) = do
  theta <- subtype ctx a1 a2
  subtype ctx (applyCtx theta b1) (applyCtx theta b2)
-- <:-->
subtype ctx (TArr a1 a2) (TArr b1 b2) = do
  theta <- subtype ctx b1 a1
  subtype theta (applyCtx theta a2) (applyCtx theta b2)
-- <:forallL
subtype ctx (TAll alpha a) b = do
  alphaHat <- freshTEVar
  let ctx' = ctx |> CMarker alphaHat |> CEVar alphaHat
      a' = tySubstitue alpha (TEVar alphaHat) a
  ctxUntil (CMarker alphaHat) <$> subtype ctx' a' b
-- <:forallR
subtype ctx a (TAll alpha b) =
  ctxUntil (CVar alpha) <$> subtype (ctx |> CVar alpha) a b
-- <:InstantiateL
subtype ctx (TEVar alphaHat) a
  | alphaHat `notElem` tyFreeTEVars a =
    instantiateL ctx alphaHat a
-- <:InstantiateR
subtype ctx a (TEVar alphaHat)
  | alphaHat `notElem` tyFreeTEVars a =
    instantiateR ctx a alphaHat
subtype _ a b =
  throw $ "cannot establish subtyping with " ++ show a ++ " <: " ++ show b

instantiateL :: TypeCheck r => Context -> TEVar -> Type -> Sem r Context
-- InstLSolve
instantiateL ctx ea ty
  | isMono ty,
    Just (gamma, gamma') <- ctxHole (CEVar ea) ctx,
    typeWellForm gamma ty =
    pure $ gamma |> CSolve ea ty <> gamma'
-- InstLReach
instantiateL ctx ea (TEVar eb)
  | Just (l, m, r) <- ctxHole2 (CEVar ea) (CEVar eb) ctx =
    pure $ l |> CEVar ea <> m |> CSolve eb (TEVar ea) <> r
-- InstLArr
instantiateL ctx ea (TArr a1 a2) | Just (l, r) <- ctxHole (CEVar ea) ctx = do
  ea1 <- freshTEVar
  ea2 <- freshTEVar
  theta <-
    instantiateR
      (l |> CEVar ea2 |> CEVar ea1 |> CSolve ea (TArr (TEVar ea1) (TEVar ea2)) <> r)
      a1
      ea1
  instantiateL theta ea2 (applyCtx theta a2)
-- InstLAllR
instantiateL ctx ea (TAll beta b) =
  ctxUntil (CVar beta) <$> instantiateL (ctx |> CVar beta) ea b
instantiateL ctx ea ty =
  throw $ "cannot instantiate " ++ show ea ++ " with " ++ show ty

-- | Under input context gamma, instantiate ea such that A <: ea, with output context delta
instantiateR :: TypeCheck r => Context -> Type -> TEVar -> Sem r Context
-- InstRSolve
instantiateR ctx ty ea
  | isMono ty,
    Just (gamma, gamma') <- ctxHole (CEVar ea) ctx,
    typeWellForm gamma ty =
    pure $ gamma |> CSolve ea ty <> gamma'
-- InstRReach
instantiateR ctx (TEVar eb) ea
  | Just (l, m, r) <- ctxHole2 (CEVar ea) (CEVar eb) ctx =
    pure $ l |> CEVar ea <> m |> CSolve eb (TEVar ea) <> r
-- InstRArr
instantiateR ctx (TArr a1 a2) ea | Just (l, r) <- ctxHole (CEVar ea) ctx = do
  ea1 <- freshTEVar
  ea2 <- freshTEVar
  theta <-
    instantiateL
      (l |> CEVar ea2 |> CEVar ea1 |> CSolve ea (TArr (TEVar ea1) (TEVar ea2)) <> r)
      ea1
      a1
  instantiateR theta (applyCtx theta a2) ea2
-- InstRAllL
instantiateR ctx (TAll beta b) ea = do
  eb <- freshTEVar
  let ctx' = ctx |> CMarker eb |> CEVar eb
  ctxUntil (CMarker eb) <$> instantiateR ctx' (tySubstitue beta (TEVar eb) b) ea
instantiateR ctx ty eb =
  throw $ "cannot instantiate " ++ show ty ++ " with " ++ show eb

synthesize :: TypeCheck r => Context -> Expr -> Sem r (Type, Context)
-- Var
synthesize ctx (EVar x) | Just ty <- ctxAssump ctx x = pure (ty, ctx)
-- Anno
synthesize ctx (EAnno e ty) | typeWellForm ctx ty = (,) ty <$> check ctx e ty
-- 1I ==>
synthesize ctx EUnit = pure (TUnit, ctx)
-- True ==>
synthesize ctx ETrue = pure (TBool, ctx)
-- False ==>
synthesize ctx EFalse = pure (TBool, ctx)
-- ZeroI ==>
synthesize ctx EZero = pure (TNat, ctx)
-- Succ =>
synthesize ctx (ESucc n) = do
  theta <- check ctx n TNat
  return (TNat, theta)
-- NatCase ==>
synthesize ctx (ENatCase n e1 x e2) = do
  theta <- check ctx n TNat
  (a, delta) <- synthesize theta e1
  eb <- freshTEVar
  sigma <- check (delta |> CEVar eb |> CAssump x TNat) e2 (TEVar eb)
  psi <- subtype sigma a (TEVar eb)
  chi <- subtype psi (TEVar eb) a
  return (TEVar eb, ctxUntil (CAssump x TNat) chi)
-- Prod==>
synthesize ctx (EProd e1 e2) = do
  (a, theta) <- synthesize ctx e1
  (b, delta) <- synthesize theta e2
  return (TProd a b, delta)
-- Proj1==>
synthesize ctx (EProj1 e) = do
  (prod, theta) <- synthesize ctx e
  case applyCtx theta prod of
    TProd a _ -> return (a, theta)
    _ -> throw $ "cannot do projection on type: " ++ show prod
-- Proj2==>
-- Proj1==>
synthesize ctx (EProj2 e) = do
  (prod, theta) <- synthesize ctx e
  case applyCtx theta prod of
    TProd _ b -> return (b, theta)
    _ -> throw $ "cannot do projection on type: " ++ show prod
-- -->I==>
synthesize ctx (ELam x e) = do
  ea <- freshTEVar
  eb <- freshTEVar
  let ctx' = ctx |> CEVar ea |> CEVar eb |> CAssump x (TEVar ea)
  delta <- ctxUntil (CAssump x (TEVar ea)) <$> check ctx' e (TEVar eb)
  return (TArr (TEVar ea) (TEVar eb), delta)
-- A-->I==>
synthesize ctx (EALam x ty e) = do
  eb <- freshTEVar
  let ctx' = ctx |> CEVar eb |> CAssump x ty
  delta <- ctxUntil (CAssump x ty) <$> check ctx' e (TEVar eb)
  return (TArr ty (TEVar eb), delta)
-- -->E
synthesize ctx (EApp e1 e2) = do
  (a, theta) <- synthesize ctx e1
  apply theta (applyCtx theta a) e2
-- Let==>
synthesize ctx (ELet x e1 e2) = do
  eb <- freshTEVar
  let ctx' = ctx |> CEVar eb
  (a, theta) <- synthesize ctx' e1
  let a' = applyCtx theta a
  delta <- check (theta |> CAssump x a') e2 (TEVar eb)
  return (TEVar eb, ctxUntil (CAssump x a') delta)
-- ALet==>
synthesize ctx (EALet x ty e1 e2) = do
  eb <- freshTEVar
  theta <- check (ctx |> CEVar eb) e1 ty
  let xAssump = CAssump x (applyCtx theta ty)
  delta <- check (theta |> xAssump) e2 (TEVar eb)
  return (TEVar eb, ctxUntil xAssump delta)
-- If==>
synthesize ctx (EIf b e1 e2) = do
  theta <- check ctx b TBool
  (a, delta) <- synthesize theta e1
  (b, sigma) <- synthesize delta e2
  -- inferred types should be subtype to each other
  psi <- subtype sigma a b
  chi <- subtype psi b a
  return (applyCtx chi a, chi)
synthesize ctx (EFix e) = do
  (ty, theta) <- synthesize ctx e
  case applyCtx theta ty of
    TArr a b | a == b -> do
      delta <- subtype theta (applyCtx theta a) (applyCtx theta b)
      sigma <- subtype delta (applyCtx delta a) (applyCtx delta b)
      return (applyCtx sigma a, sigma)
    _ -> throw $ "cannot synthesize fixpoint: " ++ show (EFix e)
synthesize ctx e = throw $ "cannot synthesize expression " ++ show e

check :: TypeCheck r => Context -> Expr -> Type -> Sem r Context
-- 1I
check ctx EUnit TUnit = pure ctx
-- TrueI
check ctx ETrue TBool = pure ctx
-- FalseI
check ctx EFalse TBool = pure ctx
-- ZeroI
check ctx EZero TNat = pure ctx
-- Succ
check ctx (ESucc n) TNat = check ctx n TNat
-- NatCase
check ctx (ENatCase n e1 x e2) ty = do
  theta <- check ctx n TNat
  delta <- check theta e1 (applyCtx theta ty)
  ctxUntil (CAssump x TNat)
    <$> check (delta |> CAssump x TNat) e2 (applyCtx delta ty)
-- Prod
check ctx (EProd e1 e2) (TProd a b) = do
  theta <- check ctx e1 a
  check theta e2 (applyCtx theta b)
-- Proj1
check ctx (EProj1 e) ty = do
  (prod, theta) <- synthesize ctx e
  case prod of
    TProd a _ -> subtype theta (applyCtx theta a) (applyCtx theta ty)
    _ -> throw $ "cannot do projection on type: " ++ show prod
-- Proj2
check ctx (EProj2 e) ty = do
  (prod, theta) <- synthesize ctx e
  case prod of
    TProd _ a -> subtype theta (applyCtx theta a) (applyCtx theta ty)
    _ -> throw $ "cannot do projection on type: " ++ show prod
-- ForallI
check ctx e (TAll alpha a) =
  ctxUntil (CVar alpha) <$> check (ctx |> CVar alpha) e a
-- -->I
check ctx (ELam x e) (TArr a b) =
  ctxUntil (CAssump x a) <$> check (ctx |> CAssump x a) e b
-- A-->I
check ctx (EALam x ty e) (TArr a b) = do
  theta <- subtype ctx a ty
  let ty' = applyCtx theta ty
  delta <- check (theta |> CAssump x ty') e (applyCtx theta b)
  return $ ctxUntil (CAssump x ty') delta
-- Let
check ctx (ELet x e1 e2) b = do
  (a, theta) <- synthesize ctx e1
  let a' = applyCtx theta a
  ctxUntil (CAssump x a') <$> check (theta |> CAssump x a') e2 b
-- ALet
check ctx (EALet x ty e1 e2) b = do
  theta <- check ctx e1 ty
  ctxUntil (CAssump x ty) <$> check (theta |> CAssump x ty) e2 b
-- If
check ctx (EIf b e1 e2) ty = do
  theta <- check ctx b TBool
  delta <- check theta e1 ty
  check delta e2 ty
-- Fix
check ctx (EFix e) ty = check ctx e $ TArr ty ty
-- Sub
check ctx e b = do
  (a, theta) <- synthesize ctx e
  subtype theta (applyCtx theta a) (applyCtx theta b)

apply :: TypeCheck r => Context -> Type -> Expr -> Sem r (Type, Context)
-- ForallApp
apply ctx (TAll alpha a) e = do
  ea <- freshTEVar
  apply (ctx |> CEVar ea) (tySubstitue alpha (TEVar ea) a) e
-- eaApp
apply ctx (TEVar ea) e | Just (l, r) <- ctxHole (CEVar ea) ctx = do
  ea1 <- freshTEVar
  ea2 <- freshTEVar
  delta <-
    check
      (l |> CEVar ea2 |> CEVar ea1 |> CSolve ea (TArr (TEVar ea1) (TEVar ea2)) <> r)
      e
      (TEVar ea1)
  return (TEVar ea2, delta)
-- -->App
apply ctx (TArr a c) e = do
  delta <- check ctx e a
  return (c, delta)
apply ctx e1 e2 =
  throw $ "cannot infer type after applying " ++ show e1 ++ " with " ++ show e2

typecheck :: Expr -> Either String (Type, Context)
typecheck expr = do
  (ty, ctx) <-
    run . runError . evalState initCheckState $ synthesize mempty expr
  return (applyCtx ctx ty, ctx)
