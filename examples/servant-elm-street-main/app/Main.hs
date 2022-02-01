module Main (main) where

import Data.Text (Text)
import Data.Text.IO as T (writeFile)
import Elm.Print (showDoc)
import ExampleAPI (userAPI)
import Prettyprinter (vsep)
import ServantElm (elmForAPI)

queryFile :: Text
queryFile = showDoc $ vsep (elmForAPI userAPI)

main :: IO ()
main = do
  T.writeFile "api.elm" queryFile