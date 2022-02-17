{-# LANGUAGE BangPatterns #-}
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
    :<|> "signup" :> ReqBody '[JSON] User :> Post '[JSON] User
    :<|> "headers" :> Headers

type SimpleRequests =
  "list" :> Get '[JSON] [User]
    :<|> "customType" :> Get '[JSON] User

type Headers =
  "basic" :> Header "someHeader" Text :> Post '[JSON] Text
    :<|> "multiple" :> Header "someHeader1" Text :> Header' '[Required] "someHeader2" Int :> Post '[JSON] Text
    :<|> "customType" :> Header "sortBy" SortBy :> Post '[JSON] SortBy

data User = User
  { name :: Text,
    age :: Int
  }
  deriving (Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet User

data SortBy = Age | Name
  deriving (Show, Generic, Elm)

instance ToJSON SortBy

type Types =
  '[ User,
     SortBy
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
  simpleRequests
    :<|> return
    :<|> headers

simpleRequests :: Server SimpleRequests
simpleRequests =
  return users
    :<|> return albert

headers :: Server Headers
headers =
  headerValue
    :<|> multipleHeadersValue
    :<|> customTypeHeaderValue

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
