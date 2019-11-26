module Main where


import           Example
import           Static.TypeCheck               ( typecheck )

main :: IO ()
main = do
  print $ typecheck id'
  print $ typecheck idIdAnno
  print $ typecheck idUnit
  print $ typecheck idUnit'
  print $ typecheck nestedId
  print $ typecheck nestedIdUnit
  print $ typecheck nestedIdId
  print $ typecheck nestedIdIdUnit
