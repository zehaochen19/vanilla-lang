module Main where


import           Example
import           Static.TypeCheck               ( typecheck )

main :: IO ()
main = do
  print $ typecheck id'
  print $ typecheck idUnit
