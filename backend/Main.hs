module Main where

import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Vanilla.Server

main :: IO ()
main =
  withStdoutLogger $ \logger ->
    let settings = setPort 8080 $ setLogger logger defaultSettings
     in runSettings settings app
