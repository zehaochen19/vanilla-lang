module Main where


import           SystemF.Program


main :: IO ()
main = do
  putStrLn "id'"
  print $ typecheck id'
  print $ eval id'
  putStrLn "idIdAnno"
  print $ typecheck idIdAnno
  print $ eval idIdAnno
  putStrLn "idUnit"
  print $ typecheck idUnit
  print $ eval idUnit
  putStrLn "idUnit'"
  print $ typecheck idUnit'
  print $ eval idUnit'
  putStrLn "nestedId"
  print $ typecheck nestedId
  print $ eval nestedId
  putStrLn "nestedIdId"
  print $ typecheck nestedIdId
  print $ eval nestedIdId
  putStrLn "nestedIdUnit"
  print $ typecheck nestedIdUnit
  print $ eval nestedIdUnit
  putStrLn "nestedIdIdUnit"
  print $ typecheck nestedIdIdUnit
  print $ eval nestedIdIdUnit
  putStrLn "letIdUnit"
  print $ typecheck letIdUnit
  print $ eval letIdUnit
  putStrLn "letNestedIdId"
  print $ typecheck letNestedIdId
  print $ eval letNestedIdId
  putStrLn "letNestedIdUnit"
  print $ typecheck letNestedIdUnit
  print $ eval letNestedIdUnit
  putStrLn "mistypedLetNestedUnit"
  print $ typecheck illtypedLetNestedUnit
  print $ eval illtypedLetNestedUnit
  putStrLn "lambdaIdIdUnit"
  print $ typecheck lambdaIdIdUnit
  print $ eval lambdaIdIdUnit
