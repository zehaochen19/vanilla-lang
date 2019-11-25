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

newtype Context = Context (S.Seq CtxMember)


(|>) :: Context -> CtxMember -> Context
(Context gamma) |> c = Context $ gamma S.|> c

ctxElem :: CtxMember -> Context -> Bool
ctxElem c (Context gamma) = c `elem` gamma

ctxHole :: Context -> CtxMember -> Maybe (Context, Context)
ctxHole (Context gamma) c = if c `elem` gamma
  then Just (Context a, Context $ S.drop 1 b)
  else Nothing
  where (a, b) = (== c) `S.breakl` gamma

ctxHole2
  :: Context -> CtxMember -> CtxMember -> Maybe (Context, Context, Context)
ctxHole2 ctx c1 c2 = do
  (a, ctx') <- ctxHole ctx c1
  (b, c   ) <- ctxHole ctx' c2
  return (a, b, c)


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
