{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vanilla.Static.Context where

import qualified Data.Sequence                 as S
import           Vanilla.Syntax.Cons            ( ConsVar )
import           Vanilla.Syntax.Expr            ( EVar )
import           Vanilla.Syntax.Type            ( TEVar
                                                , TVar
                                                , Type(..)
                                                )

data CtxMember
  = CVar TVar
  | CAssump EVar Type
  | CCons ConsVar Type
  | CEVar TEVar
  | CSolve TEVar Type
  | CMarker TEVar
  deriving (Eq, Show)

newtype Context = Context (S.Seq CtxMember) deriving (Eq, Show, Semigroup, Monoid)

(|>) :: Context -> CtxMember -> Context
(Context gamma) |> c = Context $ gamma S.|> c

ctxElem :: CtxMember -> Context -> Bool
ctxElem c (Context gamma) = c `elem` gamma

ctxHole :: CtxMember -> Context -> Maybe (Context, Context)
ctxHole c (Context gamma) = if c `elem` gamma
  then Just (dropR $ Context l, Context r)
  else Nothing
  where (r, l) = (== c) `S.breakr` gamma

ctxHole2
  :: CtxMember -> CtxMember -> Context -> Maybe (Context, Context, Context)
ctxHole2 c1 c2 ctx = do
  (ctx', c) <- ctxHole c2 ctx
  (a   , b) <- ctxHole c1 ctx'
  return (a, b, c)

dropR :: Context -> Context
dropR (Context gamma) = case S.viewr gamma of
  S.EmptyR      -> Context S.empty
  gamma' S.:> _ -> Context gamma'

ctxUntil :: CtxMember -> Context -> Context
ctxUntil c (Context gamma) = dropR . Context $ S.dropWhileR (/= c) gamma

-- | Find the solution of alpha hat in the context
ctxSolve :: Context -> TEVar -> Maybe Type
ctxSolve (Context gamma) alpha = loop gamma
 where
  loop S.Empty        = Nothing
  loop (_ S.:|> CSolve alpha' ty) | alpha == alpha' = Just ty
  loop (ctx' S.:|> _) = loop ctx'

ctxAssump :: Context -> EVar -> Maybe Type
ctxAssump (Context gamma) x = loop gamma
 where
  loop S.Empty                           = Nothing
  loop (_ S.:|> CAssump x' ty) | x == x' = Just ty
  loop (ctx' S.:|> _)                    = loop ctx'

ctxCons :: Context -> ConsVar -> Maybe Type
ctxCons (Context gamma) c = loop gamma
 where
  loop S.Empty                         = Nothing
  loop (_ S.:|> CCons c' ty) | c == c' = Just ty
  loop (ctx' S.:|> _)                  = loop ctx'

-- | Applying a context, as a substitution, to a type
ctxApply :: Context -> Type -> Type
ctxApply gamma ty = case ty of
  TVar  _         -> ty
  TEVar alpha     -> maybe ty (ctxApply gamma) $ ctxSolve gamma alpha
  TArr  a     b   -> TArr (ctxApply gamma a) (ctxApply gamma b)
  TAll  alpha a   -> TAll alpha $ ctxApply gamma a
  TData d     pat -> TData d (ctxApply gamma <$> pat)
