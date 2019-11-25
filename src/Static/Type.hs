module Static.Type
  ( Type(..)
  , isMono
  )
where


-- | A, B, C
newtype TVar = MkTVar String deriving (Eq, Show)

-- | Existential types. alpha, beta. 
newtype TEVar = MkTEVar String deriving (Eq, Show)

data Type
  = TUnit
  | TVar TVar
  | TEVar TEVar
  | TArr Type Type
  | TAll TEVar Type
  deriving(Eq, Show)

-- | Monotypes: tau, sigma.
isMono :: Type -> Bool
isMono TUnit      = True
isMono (TVar  _ ) = True
isMono (TEVar _ ) = True
isMono (TArr a b) = isMono a && isMono b
isMono _          = False
