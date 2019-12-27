{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Vanilla.Static.TypeCheck.StaticError where

import           Polysemy
import           Polysemy.Error
import           Vanilla.Syntax.Cons            ( ConsVar )
import           Vanilla.Syntax.Expr
import           Vanilla.Syntax.Type

newtype StaticError
  = TypeCheckError TypeCheckError

data TypeCheckError
  = SubtypeError Type Type
  | IllformedError Type
  | InstantiateLError TEVar Type
  | InstantiateRError Type TEVar
  | EmptyBranchError Expr
  | CannotPatternMatch Type
  | ApplyOnNonPolyType Type
  | SynthesizeError Expr
  | CheckError Expr Type
  | ApplyError Type Expr
  | UndefinedConstructor ConsVar
  deriving (Eq)

instance Show StaticError where
  show (TypeCheckError e) = show e

instance Show TypeCheckError where
  show (SubtypeError t1 t2) =
    "cannot establish subtyping with " ++ show t1 ++ " <: " ++ show t2
  show (IllformedError ty) = "ill-formed type: " ++ show ty
  show (InstantiateLError ea ty) =
    "cannot instantiate " ++ show ea ++ " with " ++ show ty
  show (InstantiateRError ty eb) =
    "cannot instantiate " ++ show ty ++ " with " ++ show eb
  show (EmptyBranchError e) = "empty branch in pattern match: " ++ show e
  show (CannotPatternMatch ty) =
    "cannot use pattern match on type: " ++ show ty
  show (ApplyOnNonPolyType ty) =
    "cannot apply type to non-poly type: " ++ show ty
  show (SynthesizeError e) = "cannot synthesize expression: " ++ show e
  show (CheckError e ty) =
    "cannot check expression: e" ++ show e ++ " with type: " ++ show ty
  show (ApplyError ty e) =
    "cannot infer type after applying " ++ show ty ++ " with " ++ show e
  show (UndefinedConstructor cons) =
    "undefined data constructor: " ++ show cons

throwTyErr :: Member (Error StaticError) r => TypeCheckError -> Sem r a
throwTyErr = throw . TypeCheckError
