{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Syntax.Type
  ( Type(..)
  , TVar(..)
  , TEVar(..)
  , isMono
  , tyFreeTEVars
  , tvar
  , (-->)
  )
where

import           Data.Set                      as S
import           Data.String                    ( IsString )

-- | A, B, C
newtype TVar = MkTVar String deriving (Eq, Show, Ord, IsString)

tvar :: String -> TVar
tvar = MkTVar

-- | Existential types. alpha, beta. 
newtype TEVar = MkTEVar String deriving (Eq, Show, Ord, IsString)

data Type
  = TUnit
  | TVar TVar
  | TEVar TEVar
  | TArr Type Type
  | TAll TVar Type
  deriving(Eq, Show)


infixr 2 -->
(-->) :: Type -> Type -> Type
(-->) = TArr


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
