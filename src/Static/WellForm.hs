module Static.WellForm where

import Data.Maybe (isJust)
import Static.Context
import Syntax.Type

typeWellForm :: Context -> Type -> Bool
typeWellForm ctx (TVar alpha) = CVar alpha `ctxElem` ctx
typeWellForm ctx TUnit = True
typeWellForm ctx TBool = True
typeWellForm ctx TNat = True
typeWellForm ctx (TArr a b) = typeWellForm ctx a && typeWellForm ctx b
typeWellForm ctx (TAll alpha a) = typeWellForm (ctx |> CVar alpha) a
typeWellForm ctx (TEVar ea) = CEVar ea `ctxElem` ctx || isJust (ctxSolve ctx ea)
