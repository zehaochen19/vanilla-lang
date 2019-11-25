module Lib
  ( someFunc
  )
where

import           Static.Type
import           Static.Expr
import           Static.Context

someFunc :: IO ()
someFunc = putStrLn "someFunc"
