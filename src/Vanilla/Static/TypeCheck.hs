{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Vanilla.Static.TypeCheck where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable                  ( foldlM
                                                , toList
                                                )
import           Data.Sequence                  ( Seq )
import           Debug.Trace
import           Vanilla.Static.Context
import           Vanilla.Static.TypeCheck.DeclCheck
import           Vanilla.Static.TypeCheck.Internal
import           Vanilla.Static.TypeCheck.WellForm
import           Vanilla.Syntax.Expr            ( Branch(..)
                                                , Expr(..)
                                                )
import           Vanilla.Syntax.Program
import           Vanilla.Syntax.Type            ( TEVar(..)
                                                , Type(..)
                                                , isMono
                                                , tyFreeTEVars
                                                , tySubstitue
                                                )

subtype :: TypeCheck m => Context -> Type -> Type -> m Context
-- <:Var
subtype ctx (TVar a) (TVar a') | a == a'                   = pure ctx
-- <:ExVar
subtype ctx (TEVar alpha) (TEVar alpha') | alpha == alpha' = pure ctx
-- <:-->
subtype ctx (TArr a1 a2) (TArr b1 b2)                      = do
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
-- <:DataType
-- For data types, the instantiated type pair should be equivalent
-- TODO: consider variances in data types
subtype ctx (TData t1 pat1) (TData t2 pat2) | t1 == t2 = do
  let pat1' = toList pat1
  let pat2' = toList pat2
  ctx' <- foldlM subtypePair ctx $ zip pat1' pat2'
  foldlM subtypePair ctx' $ zip pat2' pat1'
  where subtypePair c (ty1, ty2) = subtype c (applyCtx c ty1) (applyCtx c ty2)
-- <:InstantiateL
subtype ctx (TEVar alphaHat) a | alphaHat `notElem` tyFreeTEVars a =
  instantiateL ctx alphaHat a
-- <:InstantiateR
subtype ctx a (TEVar alphaHat) | alphaHat `notElem` tyFreeTEVars a =
  instantiateR ctx a alphaHat
subtype _ a b = throwTyErr $ SubtypeError a b

instantiateL :: TypeCheck m => Context -> TEVar -> Type -> m Context
-- InstLReach
instantiateL ctx ea (TEVar eb)
  | Just (l, m, r) <- ctxHole2 (CEVar ea) (CEVar eb) ctx
  = pure $ l |> CEVar ea <> m |> CSolve eb (TEVar ea) <> r
-- InstLArr
instantiateL ctx ea (TArr a1 a2) | Just (l, r) <- ctxHole (CEVar ea) ctx = do
  ea1   <- freshTEVar
  ea2   <- freshTEVar
  theta <- instantiateR
    (  l
    |> CEVar ea2
    |> CEVar ea1
    |> CSolve ea (TArr (TEVar ea1) (TEVar ea2))
    <> r
    )
    a1
    ea1
  instantiateL theta ea2 (applyCtx theta a2)
-- InstLAllR
instantiateL ctx ea (TAll beta b) =
  ctxUntil (CVar beta) <$> instantiateL (ctx |> CVar beta) ea b
-- InstLSolve
instantiateL ctx ea ty
  | isMono ty, Just (gamma, gamma') <- ctxHole (CEVar ea) ctx = do
    gamma |- ty
    pure $ gamma |> CSolve ea ty <> gamma'
instantiateL ctx ea ty = throwTyErr $ InstantiateLError ea ty

-- | Under input context gamma, instantiate ea such that A <: ea, with output context delta
instantiateR :: TypeCheck m => Context -> Type -> TEVar -> m Context
-- InstRReach
instantiateR ctx (TEVar eb) ea
  | Just (l, m, r) <- ctxHole2 (CEVar ea) (CEVar eb) ctx
  = pure $ l |> CEVar ea <> m |> CSolve eb (TEVar ea) <> r
-- InstRArr
instantiateR ctx (TArr a1 a2) ea | Just (l, r) <- ctxHole (CEVar ea) ctx = do
  ea1   <- freshTEVar
  ea2   <- freshTEVar
  theta <- instantiateL
    (  l
    |> CEVar ea2
    |> CEVar ea1
    |> CSolve ea (TArr (TEVar ea1) (TEVar ea2))
    <> r
    )
    ea1
    a1
  instantiateR theta (applyCtx theta a2) ea2
-- InstRAllL
instantiateR ctx (TAll beta b) ea = do
  eb <- freshTEVar
  let ctx' = ctx |> CMarker eb |> CEVar eb
  ctxUntil (CMarker eb) <$> instantiateR ctx' (tySubstitue beta (TEVar eb) b) ea
-- InstRSolve
instantiateR ctx ty ea
  | isMono ty, Just (gamma, gamma') <- ctxHole (CEVar ea) ctx = do
    gamma |- ty
    pure $ gamma |> CSolve ea ty <> gamma'
instantiateR ctx ty eb = throwTyErr $ InstantiateRError ty eb

synthesize :: TypeCheck m => Context -> Expr -> m (Type, Context)
-- Var
synthesize ctx (EVar x) | Just ty <- ctxAssump ctx x = pure (ty, ctx)
-- Cons
synthesize ctx (ECons name mempty) | Just ty <- ctxCons ctx name =
  pure (ty, ctx)
synthesize ctx e'@(ECase e branches) = do
  (ty, theta) <- synthesize ctx e
  let ty' = applyCtx theta ty
  case ty' of
    TData cName types -> do
      (branchTys, delta) <- synthesizeBranch theta types branches
      sigma              <- allSubype delta branchTys
      case branchTys of
        []      -> throwTyErr $ EmptyBranchError e'
        (t : _) -> return (applyCtx sigma t, sigma)
    _ -> throwTyErr $ CannotPatternMatch ty'
-- Anno
synthesize ctx (EAnno e ty) = do
  ctx |- ty
  (,) ty <$> check ctx e ty
-- TyApp ==>
synthesize ctx (ETApp e tyArg) = do
  (polyTy, theta) <- synthesize ctx e
  case polyTy of
    TAll tv ty' -> return (applyCtx theta $ tySubstitue tv tyArg ty', theta)
    _           -> throwTyErr $ ApplyOnNonPolyType polyTy
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
  (a, theta) <- synthesize ctx e1
  let a' = applyCtx theta a
  (b, delta) <- synthesize (theta |> CAssump x a') e2
  return (applyCtx delta b, ctxUntil (CAssump x a') delta)
-- ALet==>
synthesize ctx (EALet x ty e1 e2) = do
  theta <- check ctx e1 ty
  let xAssump = CAssump x (applyCtx theta ty)
  (b, delta) <- synthesize (theta |> xAssump) e2
  return (applyCtx delta b, ctxUntil xAssump delta)
synthesize ctx (EALetRec x ty e1 e2) = do
  theta      <- check (ctx |> CAssump x ty) e1 ty
  (b, delta) <- synthesize theta e2
  return (applyCtx delta b, ctxUntil (CAssump x ty) delta)
synthesize ctx e'@(EFix e) = do
  (ty, theta) <- synthesize ctx e
  case applyCtx theta ty of
    TArr a b | a == b -> do
      delta <- subtype theta (applyCtx theta a) (applyCtx theta b)
      sigma <- subtype delta (applyCtx delta a) (applyCtx delta b)
      return (applyCtx sigma a, sigma)
    _ -> throwTyErr $ SynthesizeError e'
synthesize ctx e = throwTyErr $ SynthesizeError e

check :: TypeCheck m => Context -> Expr -> Type -> m Context
-- Case
check ctx (ECase e branches) target = do
  (ty, theta) <- synthesize ctx e
  let ty' = applyCtx theta ty
  case ty' of
    TData cName types -> checkBranch ctx types branches target
    _                 -> throwTyErr $ CannotPatternMatch ty'
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
check ctx (EALetRec x ty e1 e2) b = do
  theta <- check (ctx |> CAssump x ty) e1 ty
  ctxUntil (CAssump x ty) <$> check theta e2 (applyCtx theta b)
-- Fix
check ctx (EFix e       ) ty = check ctx e $ TArr ty ty
-- TyApp
check ctx (ETApp e tyArg) ty = do
  (polyTy, theta) <- synthesize ctx e
  case polyTy of
    TAll tv ty' -> subtype theta
                           (applyCtx theta $ tySubstitue tv tyArg ty')
                           (applyCtx theta ty)
    _ -> throwTyErr $ ApplyOnNonPolyType polyTy
-- Sub
check ctx e b = do
  (a, theta) <- synthesize ctx e
  subtype theta (applyCtx theta a) (applyCtx theta b)

apply :: TypeCheck m => Context -> Type -> Expr -> m (Type, Context)
-- ForallApp
apply ctx (TAll alpha a) e = do
  ea <- freshTEVar
  apply (ctx |> CEVar ea) (tySubstitue alpha (TEVar ea) a) e
-- eaApp
apply ctx (TEVar ea) e | Just (l, r) <- ctxHole (CEVar ea) ctx = do
  ea1   <- freshTEVar
  ea2   <- freshTEVar
  delta <- check
    (  l
    |> CEVar ea2
    |> CEVar ea1
    |> CSolve ea (TArr (TEVar ea1) (TEVar ea2))
    <> r
    )
    e
    (TEVar ea1)
  return (TEVar ea2, delta)
-- -->App
apply ctx (TArr a c) e = do
  delta <- check ctx e a
  return (c, delta)
apply ctx ty1 e2 = do
  trace (show ty1) $ pure ()
  trace (show e2) $ pure ()
  throwTyErr $ ApplyError ty1 e2

-- | Given context and type variables, checking
--   pattern match branches will be evaluated to a target type
checkBranch
  :: TypeCheck m => Context -> Seq Type -> [Branch] -> Type -> m Context
checkBranch ctx tys [] target = pure ctx
checkBranch ctx tys (Branch cons evars e : bs) target =
  case ctxCons ctx cons of
    Nothing     -> throwTyErr $ UndefinedConstructor cons
    Just consTy -> do
      eTy <- freshTEVar
      let ctx' = ctx |> CMarker eTy <> typings
      theta <- ctxUntil (CMarker eTy) <$> check ctx' e target
      checkBranch theta tys bs target
     where
      instTy       = foldl (\(TAll tv t1) t2 -> tySubstitue tv t2 t1) consTy tys
      (_, typings) = foldl (\(TArr a b, c) x -> (b, c |> CAssump x a))
                           (instTy, mempty)
                           evars

synthesizeBranch
  :: TypeCheck m => Context -> Seq Type -> [Branch] -> m ([Type], Context)
synthesizeBranch ctx tys []                         = pure ([], ctx)
synthesizeBranch ctx tys (Branch cons evars e : bs) = case ctxCons ctx cons of
  Nothing     -> throwTyErr $ UndefinedConstructor cons
  Just consTy -> do
    eTy <- freshTEVar
    let ctx' = ctx |> CMarker eTy <> typings
    (inferred, theta) <- synthesize ctx' e
    let theta' = ctxUntil (CMarker eTy) theta
    (inferredTys, delta) <- synthesizeBranch theta' tys bs
    return (inferred : inferredTys, delta)
   where
    instTy = foldl (\(TAll tv t1) t2 -> tySubstitue tv t2 t1) consTy tys
    (_, typings) =
      foldl (\(TArr a b, c) x -> (b, c |> CAssump x a)) (instTy, mempty) evars

allSubype :: TypeCheck m => Context -> [Type] -> m Context
allSubype ctx []         = pure ctx
allSubype ctx (ty : tys) = foldlM loop ctx tys
 where
  loop gamma ty' = do
    theta <- subtype gamma (applyCtx gamma ty) (applyCtx gamma ty')
    subtype theta (applyCtx theta ty') (applyCtx theta ty)

typeCheckExpr :: MonadError StaticError m => Expr -> m (Type, Context)
typeCheckExpr expr = typeCheck $ Program [] expr

typeCheck :: MonadError StaticError m => Program -> m (Type, Context)
typeCheck Program {..} = do
  decls     <- checkDataTypes declarations
  (ty, ctx) <- flip evalStateT initCheckState . flip runReaderT decls $ do
    initCtx <- checkDecls mempty declarations
    synthesize initCtx mainExpr
  return (applyCtx ctx ty, ctx)

runTypeCheck :: Program -> Either StaticError (Type, Context)
runTypeCheck = runExcept . typeCheck

runTypeCheckExpr :: Expr -> Either StaticError (Type, Context)
runTypeCheckExpr = runExcept . typeCheckExpr
