{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Syntax.Cons where

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T

newtype ConsVar = MkConsVar Text deriving (Eq, Ord, IsString)

instance Show ConsVar where
  show (MkConsVar v) = T.unpack v
