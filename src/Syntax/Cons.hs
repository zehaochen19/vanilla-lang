{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Syntax.Cons where

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Syntax.Type (Type)

newtype ConsVar = MkConsVar Text deriving (Eq, Ord, IsString)

data Constructor = Constructor ConsVar [Type] deriving (Eq, Show)

instance Show ConsVar where
  show (MkConsVar v) = T.unpack v
