{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Static.Context where

import qualified Data.Sequence                 as S
import           Static.Type                    ( Type(..)
                                                , TVar
                                                , TEVar
                                                )
import           Static.Expr                    ( EVar )
import           Data.Maybe                     ( maybe )

data CtxMember
  = CVar TVar
  | CAsump EVar Type
  | CEVar TEVar
  | CSolve TEVar Type
  | CMarker TEVar
  deriving (Eq, Show)

newtype Context = Context (S.Seq CtxMember) deriving(Eq, Show, Semigroup, Monoid)


(|>) :: Context -> CtxMember -> Context
(Context gamma) |> c = Context $ gamma S.|> c

ctxElem :: CtxMember -> Context -> Bool
ctxElem c (Context gamma) = c `elem` gamma

ctxHole :: CtxMember -> Context -> Maybe (Context, Context)
ctxHole c (Context gamma) = if c `elem` gamma
  then Just (Context a, Context $ S.drop 1 b)
  else Nothing
  where (a, b) = (== c) `S.breakl` gamma

ctxHole2
  :: CtxMember -> CtxMember -> Context -> Maybe (Context, Context, Context)
ctxHole2 c1 c2 ctx = do
  (a, ctx') <- ctxHole c1 ctx
  (b, c   ) <- ctxHole c2 ctx'
  return (a, b, c)


ctxUntil :: CtxMember -> Context -> Context
ctxUntil c (Context gamma) = Context $ S.takeWhileL (/= c) gamma


-- | Find the solution of alpha hat in the context
ctxSolve :: Context -> TEVar -> Maybe Type
ctxSolve (Context gamma) alpha = loop gamma
 where
  loop S.Empty        = Nothing
  loop (_ S.:|> CSolve alpha' ty) | alpha == alpha' = Just ty
  loop (ctx' S.:|> _) = loop ctx'



-- | Applying a context, as a substitution, to a type
applyCtx :: Context -> Type -> Type
applyCtx gamma ty = case ty of
  TVar _       -> ty
  TUnit        -> TUnit
  TEVar alpha  -> maybe ty (applyCtx gamma) $ ctxSolve gamma alpha
  TArr a     b -> TArr (applyCtx gamma a) (applyCtx gamma b)
  TAll alpha a -> TAll alpha $ applyCtx gamma a
