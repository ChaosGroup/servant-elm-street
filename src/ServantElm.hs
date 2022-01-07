{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module ServantElm
    ( --elmForAPI
    ) where

import Prelude hiding ((<$>))
import Servant.Foreign hiding (Static)
import Data.Text hiding (concat, map)
import Elm.Generic (Elm(..))
import Elm.Ast (ElmDefinition(..))
import Data.Aeson
import Data.Proxy
import Data.Text as T (Text)
import Data.Text.IO as T (writeFile, readFile)
import Network.Wai
import qualified Data.Text as T hiding (concat, map)
import Foreign (Int)
import Servant.JS hiding (urlPrefix)
import Data.Aeson.Types (ToJSON)
import Servant (Proxy(Proxy), Get, JSON, Server, Application, serve)
import GHC.Generics (Generic)
import Servant.API
import Network.Wai.Handler.Warp (run)
import qualified Servant.Elm as Elm
import Text.PrettyPrint.Leijen.Text as PP
import qualified Data.Text.Lazy as L
import qualified Elm.Module as Elm

data LangElm

-- map ETypeDef, EType to ElmDefinition and ...?
instance Elm a => HasForeignType LangElm ElmDefinition a where
  typeFor _ _ proxyA = toElmDefinition proxyA

-- given some api type, generates the queries to endpoints as text
elmForAPI :: (HasForeign LangElm ElmDefinition api, GenerateList
                          ElmDefinition (Foreign ElmDefinition api)) => Proxy api -> ElmGenerator -> Text -- understand ElmDefinition & lang better (HasForeign docs)
elmForAPI api generator = generator $ listFromAPI (Proxy :: Proxy LangElm) (Proxy :: Proxy ElmDefinition) api

    expect =
      case request ^. reqReturnType of
        Just elmTypeExpr
          | isEmptyType options elmTypeExpr
           ->
            "Http.expectString " <> line <+> indent i "(\\x -> case x of" <> line <+>
            indent i "Err e -> toMsg (Err e)" <> line <+>
            indent i "Ok _ -> toMsg (Ok ()))"
        Just elmTypeExpr ->
          "Http.expectJson toMsg" <+> renderDecoderName elmTypeExpr
        Nothing -> error "mkHttpRequest: no reqReturnType?"