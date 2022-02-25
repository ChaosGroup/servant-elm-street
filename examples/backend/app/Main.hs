{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Elm.Generate (Settings, defaultSettings, generateElm)
import ExampleAPI (Types, app, testAPI)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import ServantElm (generateElmModule)

settings :: Settings
settings = defaultSettings "./examples/frontend/src" ["Core", "Generated"]

corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy = simpleCorsResourcePolicy {corsRequestHeaders = ["Content-Type", "someHeader", "someHeader1", "someHeader2", "sortBy"]}

main :: IO ()
main = do
  generateElm @Types settings
  generateElmModule settings testAPI
  run 8081 $ cors (const $ Just corsResourcePolicy) app
