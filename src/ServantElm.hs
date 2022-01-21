{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module ServantElm
  ( elmForAPI,
  )
where

import Data.Aeson.Types (ToJSON)
import Data.Maybe (catMaybes)
import Data.Text as T (Text, pack, replace, words)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO (readFile, writeFile)
import Data.Text.Lazy (fromStrict)
import Elm (Elm (..), prettyShowDecoder)
import Elm.Ast
import Elm.Generic (Elm (..))
import Elm.TyRep (EType, toElmType)
import Foreign (Int)
import GHC.Desugar (AnnotationWrapper)
import GHC.ExecutionStack (Location (functionName))
import GHC.Generics (Generic, to)
import Lens.Micro ((^.))
import Network.Wai ()
import Network.Wai.Handler.Warp (run)
import Servant (Application, Get, JSON, Proxy (Proxy), Server, serve)
import Servant.API
import Servant.Foreign
  ( GenerateList,
    HasForeign (Foreign),
    HasForeignType (..),
    PathSegment (unPathSegment),
    Req,
    Segment (unSegment),
    SegmentType (Cap, Static),
    camelCase,
    listFromAPI,
    path,
    reqFuncName,
    reqMethod,
    reqReturnType,
    reqUrl,
  )
import Text.PrettyPrint.Leijen.Text
  ( Doc,
    comma,
    dquotes,
    encloseSep,
    equals,
    hsep,
    indent,
    lbrace,
    lbracket,
    line,
    lparen,
    parens,
    pretty,
    punctuate,
    rbrace,
    rbracket,
    rparen,
    space,
    text,
    textStrict,
    vsep,
    (<$>),
    (<+>),
  )
import Prelude hiding ((<$>))

data LangElm

instance Elm a => HasForeignType LangElm ElmDefinition a where
  typeFor _ _ proxyA = toElmDefinition proxyA

elmForAPI ::
  ( HasForeign LangElm ElmDefinition api,
    GenerateList
      ElmDefinition
      (Foreign ElmDefinition api)
  ) =>
  Proxy api ->
  [Doc]
elmForAPI api =
  map endpointInfoToElmQuery $
    listFromAPI (Proxy :: Proxy LangElm) (Proxy :: Proxy ElmDefinition) api

elmRecord :: [Doc] -> Doc
elmRecord = encloseSep (lbrace <> space) (line <> rbrace) (comma <> space)

indent4Spaces :: Doc -> Doc
indent4Spaces = indent 4

endpointInfoToElmQuery :: Req ElmDefinition -> Doc
endpointInfoToElmQuery requestInfo =
  funcDef
  where
    funcDef =
      vsep
        [ fnName <+> ":" <+> typeSignature,
          fnName <+> args <+> equals,
          indent4Spaces elmRequest
        ]

    fnName = textStrict . camelCase $ requestInfo ^. reqFuncName

    typeSignature =
      mkTypeSignature requestInfo

    args =
      mkArgs requestInfo

    elmRequest =
      mkRequest requestInfo

elmList :: [Doc] -> Doc
elmList [] = lbracket <> rbracket
elmList ds = lbracket <+> hsep (punctuate (line <> comma) ds) <$> rbracket

stext :: Text -> Doc
stext = text . fromStrict

mkUrl :: [Segment ElmDefinition] -> Doc
mkUrl segments =
  urlBuilder
    <$> (indent4Spaces . elmList)
      (map segmentToDoc segments)
  where
    urlBuilder :: Doc
    urlBuilder = "Url.Builder.crossOrigin urlBase" :: Doc

    segmentToDoc :: Segment ElmDefinition -> Doc
    segmentToDoc s =
      case unSegment s of
        Static path ->
          dquotes (stext (unPathSegment path))
        Cap arg ->
          error
            "TODO implement-2" -- for captures, not needed now

elmTypeRefToDoc :: TypeRef -> Doc
elmTypeRefToDoc = \case
  RefPrim elmPrim -> elmPrimToDoc elmPrim
  RefCustom (TypeName typeName) -> pretty typeName

elmTypeParenDoc :: TypeRef -> Doc -- does it make sense to be a fn
elmTypeParenDoc = parens . elmTypeRefToDoc

elmPrimToDoc :: ElmPrim -> Doc
elmPrimToDoc = \case
  ElmUnit -> "()"
  ElmNever -> "Never"
  ElmBool -> "Bool"
  ElmChar -> "Char"
  ElmInt -> "Int"
  ElmFloat -> "Float"
  ElmString -> "String"
  ElmTime -> "Posix"
  ElmMaybe t -> "Maybe" <+> elmTypeParenDoc t
  ElmResult l r -> "Result" <+> elmTypeParenDoc l <+> elmTypeParenDoc r
  ElmPair a b -> lparen <> elmTypeRefToDoc a <> comma <+> elmTypeRefToDoc b <> rparen
  ElmTriple a b c -> lparen <> elmTypeRefToDoc a <> comma <+> elmTypeRefToDoc b <> comma <+> elmTypeRefToDoc c <> rparen
  ElmList l -> "List" <+> elmTypeParenDoc l

mkTypeSignature :: Req ElmDefinition -> Doc
mkTypeSignature request =
  (hsep . punctuate " ->") ("String" : catMaybes [toMsgType, returnType])
  where
    elmTypeRef :: ElmDefinition -> Doc
    elmTypeRef eDef = elmTypeRefToDoc (definitionToRef eDef)
    toMsgType :: Maybe Doc
    toMsgType = do
      result <- fmap elmTypeRef $ request ^. reqReturnType
      Just ("(Result Http.Error " <+> parens result <+> " -> msg)")

    returnType :: Maybe Doc
    returnType = do
      pure "Cmd msg"

mkArgs :: Req ElmDefinition -> Doc
mkArgs request =
  hsep ["urlBase", "toMsg"]

mkRequest :: Req ElmDefinition -> Doc
mkRequest request =
  "Http.request"
    <$> indent4Spaces
      ( elmRecord
          [ "method ="
              <$> indent4Spaces method,
            "headers ="
              <$> indent4Spaces "[]",
            "url ="
              <$> indent4Spaces url,
            "body ="
              <$> indent4Spaces "Http.emptyBody",
            "timeout ="
              <$> indent4Spaces "Nothing",
            "tracker ="
              <$> indent4Spaces "Nothing"
          ]
      )
  where
    method = pretty . pack . show $ request ^. reqMethod
    url =
      mkUrl (request ^. reqUrl . path)