{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vanilla.Syntax.Type
  ( Type (..),
    TVar (..),
    TEVar (..),
    isMono,
    tyFreeTEVars,
    tyFreeTVars,
    tvar,
    (-->),
    isTArr,
    isTAll,
    tyParen,
    tdata,
    tdata',
    tySubstitue,
  )
where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Set (Set)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T

-- | A, B, C
newtype TVar = MkTVar Text deriving (Eq, Ord, IsString)

instance Show TVar where
  show (MkTVar v) = T.unpack v

tvar :: Text -> TVar
tvar = MkTVar

-- | Existential types. alpha, beta.
newtype TEVar = MkTEVar Text deriving (Eq, Show, Ord, IsString)

data Type
  = TVar TVar
  | TEVar TEVar
  | TArr Type Type
  | TAll TVar Type
  | TData Text (Seq Type)
  deriving (Eq)

infixr 2 -->

(-->) :: Type -> Type -> Type
(-->) = TArr

tdata :: Text -> Type
tdata t = TData t mempty

tdata' :: Text -> [Type] -> Type
tdata' t tys = TData t (Seq.fromList tys)

-- | [ty1/alpha]ty2
tySubstitue :: TVar -> Type -> Type -> Type
tySubstitue alpha ty1 ty2 =
  case ty2 of
    TVar alpha' -> if alpha == alpha' then ty1 else ty2
    TEVar _ -> ty2
    TAll beta a ->
      if alpha == beta then ty2 else TAll beta (tySubstitue alpha ty1 a)
    TArr a b -> TArr (tySubstitue alpha ty1 a) (tySubstitue alpha ty1 b)
    TData d pat -> TData d (tySubstitue alpha ty1 <$> pat)

-- | Monotypes: tau, sigma.
isMono :: Type -> Bool
isMono (TVar _) = True
isMono (TEVar _) = True
isMono (TArr a b) = isMono a && isMono b
isMono (TData _ pat) = all isMono pat
isMono _ = False

tyFreeTEVars :: Type -> Set TEVar
tyFreeTEVars (TVar _) = S.empty
tyFreeTEVars (TEVar evar) = S.singleton evar
tyFreeTEVars (TArr a b) = tyFreeTEVars a <> tyFreeTEVars b
tyFreeTEVars (TAll _ ty) = tyFreeTEVars ty
tyFreeTEVars (TData _ pat) = mconcat (tyFreeTEVars <$> toList pat)

tyFreeTVars :: Type -> Set TVar
tyFreeTVars (TVar a) = S.singleton a
tyFreeTVars (TEVar _) = S.empty
tyFreeTVars (TArr a b) = tyFreeTVars a <> tyFreeTVars b
tyFreeTVars (TAll a ty) = S.delete a $ tyFreeTVars ty
tyFreeTVars (TData _ pat) = mconcat (tyFreeTVars <$> toList pat)

isTArr :: Type -> Bool
isTArr (TArr _ _) = True
isTArr _ = False

isTAll :: Type -> Bool
isTAll (TAll _ _) = True
isTAll _ = False

instance Show Type where
  show (TVar (MkTVar x)) = T.unpack x
  show (TEVar (MkTEVar x)) = "Existential " ++ T.unpack x
  show (TArr a b) = tyParen a ++ " → " ++ tyParen b
  show (TAll a ty) = "∀" ++ show a ++ ". " ++ tyParen ty
  show (TData name pat) =
    let patStr = if null pat then "" else " " ++ (unwords . toList . fmap tyParen $ pat)
     in T.unpack name ++ patStr

tyParen :: Type -> String
tyParen ty = case ty of
  TVar _ -> show ty
  TEVar _ -> show ty
  TData _ tys | null tys -> show ty
  _ -> "(" ++ show ty ++ ")"
