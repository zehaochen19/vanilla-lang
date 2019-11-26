module Static.WellForm where


import           Static.Type
import           Static.Context
import           Data.Maybe                     ( isJust )


typeWellForm :: Context -> Type -> Bool
typeWellForm ctx (TVar alpha)   = CVar alpha `ctxElem` ctx
typeWellForm ctx TUnit          = True
typeWellForm ctx (TArr a     b) = typeWellForm ctx a && typeWellForm ctx b
typeWellForm ctx (TAll alpha a) = typeWellForm (ctx |> CVar alpha) a
typeWellForm ctx (TEVar ea) =
  CEVar ea `ctxElem` ctx || isJust (ctxSolve ctx ea)
