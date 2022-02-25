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
  ( testAPI,
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
import Servant (Capture, FromHttpApiData (parseHeader, parseQueryParam), Get, Handler, Header, Header', JSON, Post, Proxy (..), QueryParam, QueryParam', QueryParams, ReqBody, Required, Server, serve, type (:<|>) (..), type (:>))

-- apis
type TestAPI =
  "simple" :> "request" :> SimpleRequests
    :<|> "body" :> BodyRequests
    :<|> "headers" :> Headers
    :<|> "query" :> "parameters" :> QueryParameters
    :<|> "captures" :> Captures

-- TestAPI subtypes
type SimpleRequests =
  "list" :> Get '[JSON] [User]
    :<|> "customType" :> Get '[JSON] User

type BodyRequests =
  "signup" :> ReqBody '[JSON] User :> Post '[JSON] User

type Headers =
  "basic" :> Header "someHeader" Text :> Post '[JSON] Text
    :<|> "multiple" :> Header "someHeader1" Text :> Header' '[Required] "someHeader2" Int :> Post '[JSON] Text
    :<|> "customType" :> Header "sortBy" SortBy :> Post '[JSON] SortBy

type QueryParameters =
  "single" :> QueryParam "name" Text :> Get '[JSON] [User]
    :<|> "two" :> QueryParam "name" Text :> QueryParam "age" Int :> Get '[JSON] [User]
    :<|> "required" :> QueryParam' '[Required] "name" Text :> Get '[JSON] [User]
    :<|> "custom" :> "flag" :> QueryParam "author" Bool :> Get '[JSON] [User]
    :<|> "list" :> QueryParams "ages" Int :> Get '[JSON] [Int]
    :<|> "mixed" :> QueryParam "age" Text :> QueryParam "name" Text :> QueryParams "authors" Text :> Get '[JSON] Text

type Captures =
  "single" :> Capture "pointId" Int :> Get '[JSON] Point
    :<|> "multiple" :> Capture "x" Int :> Capture "y" Int :> Capture "z" Int :> Get '[JSON] [Point]

-- data types
data User = User
  { name :: Text,
    age :: Int,
    author :: Bool
  }
  deriving (Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet User

data Point = Point
  { pointId :: Int,
    x :: Int,
    y :: Int,
    z :: Int
  }
  deriving (Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet Point

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

-- elm-street types
type Types =
  '[ User,
     SortBy,
     Point
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

points :: [Point]
points =
  [ Point {pointId = 1, x = 1, y = 2, z = 3},
    Point {pointId = 2, x = 6, y = 7, z = 9},
    Point {pointId = 3, x = 10, y = 13, z = 15},
    Point {pointId = 4, x = 0, y = 1, z = 9},
    Point {pointId = 5, x = 70, y = 30, z = 50}
  ]

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

queryParams :: Server QueryParameters
queryParams =
  singleQueryParameter
    :<|> twoQueryParameters
    :<|> requiredQueryParameter
    :<|> customQueryFlag
    :<|> queryList
    :<|> mixedQueryParameters

captures :: Server Captures
captures =
  singleCapture
    :<|> multipleCaptures

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

-- query parameters handlers
singleQueryParameter :: Maybe Text -> Handler [User]
singleQueryParameter param = return $ case param of
  Nothing -> users
  Just nameParam -> filter (\user -> name user == nameParam) users

twoQueryParameters :: Maybe Text -> Maybe Int -> Handler [User]
twoQueryParameters nameParam ageParam = return filteredUsers
  where
    filteredUsers = filter (\user -> name user == fromMaybe "Ivan" nameParam && age user == fromMaybe 32 ageParam) users

requiredQueryParameter :: Text -> Handler [User]
requiredQueryParameter nameParam = return $ filter (\user -> name user == nameParam) users

customQueryFlag :: Maybe Bool -> Handler [User]
customQueryFlag flag = return $ filter (\user -> author user == fromMaybe False flag) users

queryList :: [Int] -> Handler [Int]
queryList = return

mixedQueryParameters :: Maybe Text -> Maybe Text -> [Text] -> Handler Text
mixedQueryParameters a b c = return $ append (fromMaybe "no value" a) $ append (fromMaybe "no value" b) (foldl append "" c)

-- captures handlers
singleCapture :: Int -> Handler Point
singleCapture idValue = return $ points !! idValue

multipleCaptures :: Int -> Int -> Int -> Handler [Point]
multipleCaptures xCoord yCoord zCoord = return $ filter (\point -> x point == xCoord && y point == yCoord && z point == zCoord) points

-- server
server :: Server TestAPI
server =
  simpleRequests
    :<|> return
    :<|> headers
    :<|> queryParams
    :<|> captures

testAPI :: Proxy TestAPI
testAPI = Proxy

app :: Application
app = serve testAPI server
