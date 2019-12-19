{-# LANGUAGE RecordWildCards #-}

module Syntax.Program where

import Syntax.Decl
import Syntax.Expr

data Program
  = Program
      { declarations :: [Declaration],
        mainExpr :: Expr
      }
  deriving (Eq)

instance Show Program where
  show Program {..} = unlines (show <$> declarations) ++ "\n" ++ show mainExpr
