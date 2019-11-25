module Static.Type
  ( Type(..)
  , TVar(..)
  , TEVar(..)
  , isMono
  , tyFreeTEVars
  )
where

import           Data.Set                      as S

-- | A, B, C
newtype TVar = MkTVar String deriving (Eq, Show, Ord)

-- | Existential types. alpha, beta. 
newtype TEVar = MkTEVar String deriving (Eq, Show, Ord)

data Type
  = TUnit
  | TVar TVar
  | TEVar TEVar
  | TArr Type Type
  | TAll TVar Type
  deriving(Eq, Show)

-- | Monotypes: tau, sigma.
isMono :: Type -> Bool
isMono TUnit      = True
isMono (TVar  _ ) = True
isMono (TEVar _ ) = True
isMono (TArr a b) = isMono a && isMono b
isMono _          = False


tyFreeTEVars :: Type -> Set TEVar
tyFreeTEVars TUnit        = S.empty
tyFreeTEVars (TVar  _   ) = S.empty
tyFreeTEVars (TEVar evar) = S.singleton evar
tyFreeTEVars (TArr a b  ) = tyFreeTEVars a <> tyFreeTEVars b
tyFreeTEVars (TAll _ ty ) = tyFreeTEVars ty
