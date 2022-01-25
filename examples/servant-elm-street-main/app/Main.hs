module Main where

import ExampleAPI (queryFile)

main :: IO ()
main = do
  T.writeFile "api.elm" queryFile