{-# LANGUAGE RecordWildCards #-}

module Vanilla.Syntax.Decl where

import           Data.List                      ( intercalate )
import qualified Data.Map                      as M
import qualified Data.Sequence                 as S
import           Data.Text                      ( Text )
import           Vanilla.Static.Context
import           Vanilla.Syntax.Cons
import           Vanilla.Syntax.Type

-- | user defined data type declaration
data Declaration
  = Declaration
      { name :: Text,
        tvars :: [TVar],
        constructors :: [Constructor]
      }
  deriving (Eq)

instance Show Declaration where
  show Declaration {..} =
    "data "
      ++ show name
      ++ " "
      ++ unwords (show <$> tvars)
      ++ " = "
      ++ intercalate " | " (show <$> constructors)

type DeclarationMap = M.Map Text Declaration

emptyDecls :: DeclarationMap
emptyDecls = mempty

consType :: Declaration -> Constructor -> Type
consType dec (Constructor conName pat) = genTy
 where
  monoTy = foldr TArr (TData (name dec) (S.fromList $ TVar <$> tvars dec)) pat
  genTy  = foldr TAll monoTy (tvars dec)

consCtxMember :: Declaration -> Constructor -> CtxMember
consCtxMember dec c@(Constructor conName pat) = CCons conName $ consType dec c

-- | initialize typing context for constructors
initDeclCtx :: [Declaration] -> Context
initDeclCtx decls = Context . S.fromList $ decls >>= \dec ->
  consCtxMember dec <$> constructors dec

