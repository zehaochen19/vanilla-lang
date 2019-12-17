module Syntax.Decl where

import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Text (Text)
import Static.Context
import Syntax.Cons (ConsVar (..))
import Syntax.Type

-- | user defined data type declaration
data Declaration
  = Declaration
      { name :: Text,
        tvars :: [TVar],
        constructors :: [Constructor]
      }
  deriving (Eq, Show)

data Constructor = Constructor Text [Type] deriving (Eq, Show)

type DeclarationMap = M.Map Text Declaration

emptyDecls :: DeclarationMap
emptyDecls = mempty

consCtxMember :: Declaration -> Constructor -> CtxMember
consCtxMember dec (Constructor conName pat) = CCons (MkConsVar conName) genTy
  where
    monoTy = foldr TArr (TData (name dec) (TVar <$> tvars dec)) pat
    genTy = foldr TAll monoTy (tvars dec)

-- | initialize typing context for constructors
initDeclCtx :: [Declaration] -> Context
initDeclCtx decls = Context . S.fromList $
  decls
    >>= \dec -> consCtxMember dec <$> constructors dec

declMap :: [Declaration] -> DeclarationMap
declMap decls = M.fromList $ fmap loop decls
  where
    loop dec@(Declaration name _ _) = (name, dec)
