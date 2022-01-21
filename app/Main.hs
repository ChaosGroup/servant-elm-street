{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.Aeson.Types (ToJSON)
import Data.Proxy
import Data.Text
import Data.Text as T (Text)
import qualified Data.Text as T
import Data.Text.IO as T (readFile, writeFile)
import Elm.Ast (ElmDefinition (..))
import Elm.Generic (Elm (..))
import Foreign (Int)
import GHC.Generics
import GHC.Generics (Generic)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp (run)
import Servant
import Servant (Application, Get, JSON, Proxy (Proxy), Server, serve)
import Servant.API
import Servant.Foreign
import Servant.JS
import ServantElm (elmForAPI)

type UserAPI =
  "users" :> Get '[JSON] [User]
    :<|> "albert" :> Get '[JSON] User

data User = User
  { name :: String,
    age :: Int
  }
  deriving (Generic, Elm)

instance ToJSON User

users :: [User]
users =
  [ User {name = "Mina", age = 18},
    User {name = "Marta", age = 21},
    User {name = "Ivan", age = 32},
    User {name = "Abba", age = 90},
    User {name = "George", age = 43}
  ]

albert :: User
albert = User {name = "Albert", age = 18}

server :: Server UserAPI
server =
  return users
    :<|> return albert

userAPI :: Proxy UserAPI
userAPI = Proxy

data LangElm

instance Elm a => HasForeignType LangElm ElmDefinition a where
  typeFor _ _ proxyA = toElmDefinition proxyA

queryFile :: Text
queryFile = displayTStrict . renderPretty 0.8 120 $ vsep (elmForAPI userAPI)

main :: IO ()
main = do
  T.writeFile "api.elm" queryFile