module Main where

import Data.Text.IO as T
import ExampleAPI (queryFile)

main :: IO ()
main = do
  T.writeFile "api.elm" queryFile