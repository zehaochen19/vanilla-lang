{-# LANGUAGE RecordWildCards #-}

module Vanilla.Syntax.Program where

import Vanilla.Syntax.Decl
import Vanilla.Syntax.Expr

data Program
  = Program
      { declarations :: [Declaration],
        mainExpr :: Expr
      }
  deriving (Eq)

instance Show Program where
  show Program {..} = unlines (show <$> declarations) ++ "\n" ++ show mainExpr
