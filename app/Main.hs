module Main where

import qualified Data.Text.IO as TIO
import System.Environment
import System.Exit
import Vanilla

main :: IO ()
main = do
  args <- getArgs
  case args of
    (src : _) -> do
      prog <- TIO.readFile src
      case runVanilla src prog of
        Left error -> putStrLn error
        Right (expr, ty) -> do
          putStrLn "Type:"
          print ty
          putStrLn ""
          putStrLn "Result:"
          print expr
    _ -> do
      putStrLn "Usage: vanilla [src].vn"
      exitFailure
