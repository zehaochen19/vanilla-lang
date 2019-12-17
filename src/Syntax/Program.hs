module Syntax.Program where

import Syntax.Decl
import Syntax.Expr

data Program
  = Program
      { declarations :: [Declaration],
        mainExpr :: Expr
      }
