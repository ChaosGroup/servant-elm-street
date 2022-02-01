{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ExampleAPI
  ( userAPI,
    LangElm,
    server,
  )
where

import Data.Aeson (ToJSON)
import Elm.Ast (ElmDefinition)
import Elm.Generic (Elm (..))
import GHC.Generics (Generic)
import Servant (Get, JSON, Post, Proxy (..), ReqBody, Server, type (:<|>) (..), type (:>))
import Servant.Foreign (HasForeignType (..))

data LangElm

instance Elm a => HasForeignType LangElm ElmDefinition a where
  typeFor _ _ proxyA = toElmDefinition proxyA

type UserAPI =
  "users" :> Get '[JSON] [User]
    :<|> "albert" :> Post '[JSON] User
    :<|> "signup" :> ReqBody '[JSON] User :> Post '[JSON] User

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
    :<|> return

userAPI :: Proxy UserAPI
userAPI = Proxy