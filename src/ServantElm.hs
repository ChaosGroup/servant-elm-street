{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ServantElm
    ( elmForAPI
    ) where

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

data LangElm

instance Elm a => HasForeignType LangElm ElmDefinition a where
  typeFor _ _ proxyA = toElmDefinition proxyA

type ElmGenerator = [Req ElmDefinition] -> Text

-- given some api type, generates the queries to endpoints as text
elmForAPI :: (HasForeign LangElm ElmDefinition api, GenerateList
                          ElmDefinition (Foreign ElmDefinition api)) => Proxy api -> ElmGenerator -> Text -- understand ElmDefinition & lang better (HasForeign docs)
elmForAPI api generator = generator $ listFromAPI (Proxy :: Proxy LangElm) (Proxy :: Proxy ElmDefinition) api

-- receives a list of requests and returns elm query functions
generator :: ElmGenerator
generator requests = ""
