{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vanilla.Syntax.Cons where

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Vanilla.Syntax.Type (Type)

newtype ConsVar = MkConsVar Text deriving (Eq, Ord, IsString)

data Constructor = Constructor ConsVar [Type] deriving (Eq)

instance Show ConsVar where
  show (MkConsVar v) = T.unpack v

instance Show Constructor where
  show (Constructor cons tys) = show cons ++ " " ++ unwords (show <$> tys)
