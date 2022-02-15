{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Elm.Generate (Settings, defaultSettings, generateElm)
import ExampleAPI (Types, app, userAPI)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import ServantElm (generateElmModule)

settings :: Settings
settings = defaultSettings "./examples/frontend/src" ["Core", "Generated"]

corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy = simpleCorsResourcePolicy {corsRequestHeaders = ["Content-Type"]}

main :: IO ()
main = do
  generateElm @Types settings
  generateElmModule settings userAPI
  run 8081 $ cors (const $ Just corsResourcePolicy) app
