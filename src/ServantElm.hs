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
import Elm.Ast
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
                          ElmDefinition (Foreign ElmDefinition api)) => Proxy api -> [Doc]
elmForAPI api = map (endpointInfoToElmQuery defElmOptions) $ listFromAPI (Proxy :: Proxy LangElm) (Proxy :: Proxy ElmDefinition) api


defElmOptions :: ElmOptions
defElmOptions = ElmOptions
  { elmToString = defaultElmToString
  , emptyResponseElmTypes =
      [ toElmType (Proxy :: Proxy ())
      ]
  , stringElmTypes =
      [ toElmType (Proxy :: Proxy String)
      , toElmType (Proxy :: Proxy T.Text)
      ]
  }

data ElmOptions = ElmOptions {
      elmToString :: ElmDefinition -> Text
    , emptyResponseElmTypes :: [ElmDefinition]
    , stringElmTypes :: [ElmDefinition]
}

endpointInfoToElmQuery :: ElmOptions -> Req ElmDefinition -> Doc
endpointInfoToElmQuery options requestInfo =
 funcDef
  where
    funcDef =
      vsep
        [ fnName <+> ":" <+> typeSignature
        , fnName <+> args <+> equals
        , case letParams of
            Just params ->
              indent i
              (vsep ["let"
                    , indent i params
                    , "in"
                    , indent i elmRequest
                    ])
            Nothing ->
              indent i elmRequest
        ]

    fnName =
      requestInfo ^. reqFuncName . to (replace . camelCase) . to stext

    replace = T.replace "-" "" . T.replace "." ""

    typeSignature =
      mkTypeSignature options requestInfo

    args =
      mkArgs options requestInfo

    letParams =
      mkLetParams options requestInfo

    elmRequest =
      mkRequest options requestInfo

stext :: Text -> Doc
stext = text . L.fromStrict

mkTypeSignature :: ElmOptions -> Req ElmDefinition -> Doc
mkTypeSignature options request =
  (hsep . punctuate " ->") (["String"] ++ catMaybes [toMsgType, returnType])
  where

    elmTypeRef :: ElmDefinition -> Doc
    elmTypeRef eType =
      stext (toElmTypeRefWith options eType)

    toMsgType :: Maybe Doc
    toMsgType = do
      result <- fmap elmTypeRef $ request ^. reqReturnType
      Just ("(Result Http.Error " <+> parens result <+> " -> msg)")

    returnType :: Maybe Doc
    returnType = do
      pure "Cmd msg"

mkArgs :: ElmOptions -> Req ElmDefinition -> Doc
mkArgs options request =
  hsep ["toMsg"]

mkRequest :: ElmOptions -> Req ElmDefinition -> Doc
mkRequest options request =
  "Http.request" <$>
  indent i
    (elmRecord
       [ "method =" <$>
         indent i (dquotes method)
       , "headers =" <$>
         indent i "[]"
       , "url =" <$>
         indent i url
       , "body =" <$>
         indent i "Http.emptyBody"
       , "expect =" <$>
         indent i expect
       , "timeout =" <$>
         indent i "Nothing"
       , "tracker =" <$>
         indent i "Nothing"
       ])
  where
    method =
       request ^. reqMethod . to (stext . decodeUtf8)

    url =
      mkUrl options (request ^. reqUrl . path)
       <> mkQueryParams request

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