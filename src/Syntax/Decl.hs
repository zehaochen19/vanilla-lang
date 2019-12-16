module Syntax.Decl where

import Data.Text (Text)
import Syntax.Type

data Declaration
  = Declaration
      { name :: Text,
        tvars :: [TVar],
        constructors :: [Constructor]
      }

data Constructor = Constructor String [Type]
