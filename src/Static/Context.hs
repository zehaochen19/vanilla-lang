module Static.Context where

import qualified Data.Sequence                 as S
import           Static.Type                    ( Type(..)
                                                , TVar
                                                , TEVar
                                                )
import           Static.Expr                    ( EVar )


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
