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
  ( queryFile,
  )
where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Elm.Ast (ElmDefinition)
import Elm.Generic (Elm (..))
import Elm.Print (showDoc)
import GHC.Generics (Generic)
import Prettyprinter (vsep)
import Servant (Get, JSON, Proxy (..), Server, type (:<|>) (..), type (:>))
import Servant.API (Get, JSON, type (:<|>) (..), type (:>))
import Servant.Foreign (HasForeignType (..))
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
queryFile = showDoc $ vsep (elmForAPI userAPI)
