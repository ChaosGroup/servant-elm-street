{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import ServantElm --(elmForAPI)
import Data.Text
import Data.Proxy

import Servant.Foreign
import Data.Text
import Data.Proxy
import Elm.Generic (Elm(..))
import Elm.Ast (ElmDefinition(..))
import Data.Aeson
import Data.Proxy
import Data.Text as T (Text)
import Data.Text.IO as T (writeFile, readFile)
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import Servant
import Foreign (Int)
import Servant.JS
import Data.Aeson.Types (ToJSON)
import Servant (Proxy(Proxy), Get, JSON, Server, Application, serve)
import GHC.Generics (Generic)
import Servant.API
import Network.Wai.Handler.Warp (run)

type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "albert" :> Get '[JSON] User

data User = User {
    name :: String,
    age :: Int
} deriving (Generic, Elm)

instance ToJSON User

users :: [User]
users = [
    User { name="Mina", age=18 },
    User { name="Marta", age=21 },
    User { name="Ivan", age=32 },
    User { name="Abba", age=90 },
    User { name="George", age=43 }
    ]

albert :: User
albert = User { name="Albert", age=18}

server :: Server UserAPI
server = return users
    :<|> return albert

userAPI :: Proxy UserAPI
userAPI = Proxy

data LangElm

instance Elm a => HasForeignType LangElm ElmDefinition a where
  typeFor _ _ proxyA = toElmDefinition proxyA

main :: IO ()
main = do
    print $ listFromAPI (Proxy :: Proxy LangElm) (Proxy :: Proxy ElmDefinition) userAPI
     --elmForAPI userAPI