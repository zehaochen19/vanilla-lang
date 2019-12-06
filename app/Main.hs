module Main where

import qualified Data.Text.IO as TIO
import System.Environment
import System.Exit
import SystemF

main :: IO ()
main = do
  args <- getArgs
  case args of
    (src : _) -> do
      prog <- TIO.readFile src
      case runSystemF src prog of
        Left error -> putStrLn error
        Right (expr, ty) -> do
          putStrLn "Type:"
          print ty
          putStrLn ""
          putStrLn "Result:"
          print expr
    _ -> do
      putStrLn "Usage: systemf src.sf"
      exitFailure
