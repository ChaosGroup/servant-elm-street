{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
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
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text as Text (Text, append, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Elm (ElmStreet (..))
import Elm.Generic (Elm (..))
import GHC.Generics (Generic)
import Network.Wai (Application)
import Servant (FromHttpApiData (parseHeader, parseQueryParam), Get, Handler, Header, Header', JSON, Post, Proxy (..), ReqBody, Required, Server, serve, type (:<|>) (..), type (:>))

type UserAPI =
  "simple" :> "request" :> SimpleRequests
    :<|> "body" :> BodyRequests
    :<|> "headers" :> Headers

type SimpleRequests =
  "list" :> Get '[JSON] [User]
    :<|> "customType" :> Get '[JSON] User

type BodyRequests =
  "signup" :> ReqBody '[JSON] User :> Post '[JSON] User

type Headers =
  "basic" :> Header "someHeader" Text :> Post '[JSON] Text
    :<|> "multiple" :> Header "someHeader1" Text :> Header' '[Required] "someHeader2" Int :> Post '[JSON] Text
    :<|> "customType" :> Header "sortBy" SortBy :> Post '[JSON] SortBy

data User = User
  { name :: Text,
    age :: Int,
    author :: Bool
  }
  deriving (Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet User

data SortBy = Age | Name
  deriving (Show, Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet SortBy

instance FromHttpApiData SortBy where
  parseHeader :: ByteString -> Either Text SortBy
  parseHeader value = case (Text.unpack . decodeUtf8) value of
    "Age" -> Right Age
    "Name" -> Right Name
    _ -> Left "error cannot parse SortBy header value"

  parseQueryParam :: Text -> Either Text SortBy
  parseQueryParam value = case value of
    "Age" -> Right Age
    "Name" -> Right Name
    _ -> Left "error cannot parse SortBy queryParam value"

type Types =
  '[ User,
     SortBy
   ]

users :: [User]
users =
  [ User {name = "Mina", age = 18, author = False},
    User {name = "Marta", age = 21, author = True},
    User {name = "Ivan", age = 18, author = False},
    User {name = "Abba", age = 90, author = False},
    User {name = "George", age = 43, author = True}
  ]

albert :: User
albert = User {name = "Albert", age = 18, author = False}

-- server values
simpleRequests :: Server SimpleRequests
simpleRequests =
  return users
    :<|> return albert

headers :: Server Headers
headers =
  headerValue
    :<|> multipleHeadersValue
    :<|> customTypeHeaderValue

-- header handlers
customTypeHeaderValue :: Maybe SortBy -> Handler SortBy
customTypeHeaderValue = return . fromMaybe Age

multipleHeadersValue :: Maybe Text -> Int -> Handler Text
multipleHeadersValue t i =
  return $
    Text.append
      (fromMaybe "" t)
      ((Text.pack . show) i)

headerValue :: Maybe Text -> Handler Text
headerValue = return . fromMaybe "no value"

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server
