{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ExampleAPI
  ( userAPI,
    server,
    app,
    Types,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Elm (ElmStreet (..))
import Elm.Generic (Elm (..))
import GHC.Generics (Generic)
import Network.Wai (Application)
import Servant (Get, Handler, Header, JSON, Post, Proxy (..), ReqBody, Server, serve, type (:<|>) (..), type (:>))

type UserAPI =
  "users" :> Get '[JSON] [User]
    :<|> "albert" :> Get '[JSON] User
    :<|> "signup" :> ReqBody '[JSON] User :> Post '[JSON] User
    :<|> "sth" :> Header "someHeader" Text :> Post '[JSON] Text

data User = User
  { name :: Text,
    age :: Int
  }
  deriving (Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet User

type Types =
  '[ User
   ]

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
    :<|> headerValue

headerValue :: Maybe Text -> Handler Text
headerValue = return . fromMaybe "no value"

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server
