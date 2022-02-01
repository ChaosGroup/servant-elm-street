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

module ServantElm
  ( elmForAPI,
    getReqs,
  )
where

import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Text as T (pack, takeWhile)
import Elm (Elm (..))
import Elm.Ast
  ( ElmDefinition,
    ElmPrim
      ( ElmBool,
        ElmChar,
        ElmFloat,
        ElmInt,
        ElmList,
        ElmMaybe,
        ElmNever,
        ElmPair,
        ElmResult,
        ElmString,
        ElmTime,
        ElmTriple,
        ElmUnit
      ),
    TypeName (TypeName, unTypeName),
    TypeRef (..),
    definitionToRef,
  )
import Lens.Micro ((^.))
import Prettyprinter
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
    parens,
    pretty,
    punctuate,
    rbrace,
    rbracket,
    space,
    vsep,
    (<+>),
  )
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
  [Doc ann]
elmForAPI api =
  map endpointInfoToElmQuery $ getReqs api

getReqs ::
  ( HasForeign LangElm ElmDefinition api,
    GenerateList
      ElmDefinition
      (Foreign ElmDefinition api)
  ) =>
  Proxy api ->
  [Req ElmDefinition]
getReqs = listFromAPI (Proxy :: Proxy LangElm) (Proxy :: Proxy ElmDefinition)

elmRecord :: [Doc ann] -> Doc ann
elmRecord = encloseSep (lbrace <> space) (line <> rbrace) (comma <> space)

indent4Spaces :: Doc ann -> Doc ann
indent4Spaces = indent 4

endpointInfoToElmQuery :: Req ElmDefinition -> Doc ann
endpointInfoToElmQuery requestInfo =
  funcDef
  where
    funcDef =
      vsep
        [ fnName <+> ":" <+> typeSignature,
          fnName <+> args <+> equals,
          indent4Spaces elmRequest
        ]

    fnName = pretty . camelCase $ requestInfo ^. reqFuncName

    typeSignature =
      mkTypeSignature requestInfo

    args = hsep ["urlBase", "toMsg"]

    elmRequest =
      mkRequest requestInfo

elmList :: [Doc ann] -> Doc ann
elmList [] = lbracket <> rbracket
elmList ds = lbracket <+> hsep (punctuate (line <> comma) ds) <> line <> rbracket

mkUrl :: [Segment ElmDefinition] -> Doc ann
mkUrl segments =
  urlBuilder
    <> line
    <> (indent4Spaces . elmList)
      (map segmentToDoc segments)
  where
    urlBuilder :: Doc ann
    urlBuilder = "Url.Builder.crossOrigin urlBase"

    segmentToDoc :: Segment ElmDefinition -> Doc ann
    segmentToDoc s =
      case unSegment s of
        Static sPath ->
          dquotes (pretty (unPathSegment sPath))
        Cap _ ->
          error
            "to implement - for captures, not needed now"

mkTypeSignature :: Req ElmDefinition -> Doc ann
mkTypeSignature _ =
  (hsep . punctuate " ->") ("String" : catMaybes [toMsgType, returnType])
  where
    toMsgType :: Maybe (Doc ann)
    toMsgType = Just . parens $ "Result Http.Error ()" <+> "-> msg"

    returnType :: Maybe (Doc ann)
    returnType = pure "Cmd msg"

mkRequest :: Req ElmDefinition -> Doc ann
mkRequest request =
  "Http.request"
    <> line
    <> indent4Spaces
      ( elmRecord
          [ "method ="
              <> indent4Spaces method,
            "headers ="
              <> indent4Spaces "[]",
            "url ="
              <> indent4Spaces url,
            "expect ="
              <> indent4Spaces expect,
            "body ="
              <> indent4Spaces "Http.emptyBody",
            "timeout ="
              <> indent4Spaces "Nothing",
            "tracker ="
              <> indent4Spaces "Nothing"
          ]
      )
  where
    method = pretty . pack . show $ request ^. reqMethod

    url =
      mkUrl (request ^. reqUrl . path)

    expect =
      case request ^. reqReturnType of
        Just elmTypeExpr ->
          "Http.expectJson toMsg" <+> (typeRefDecoder . definitionToRef) elmTypeExpr
        Nothing -> error "mkHttpRequest: no reqReturnType?"

typeRefDecoder :: TypeRef -> Doc ann
typeRefDecoder (RefCustom TypeName {..}) = "decode" <> pretty (T.takeWhile (/= ' ') unTypeName)
typeRefDecoder (RefPrim elmPrim) = case elmPrim of
  ElmUnit -> "Json.Decode.map (always ()) (Json.Decode.list Json.Decode.string)"
  ElmNever -> "Json.Decode.fail \"Never is not possible\""
  ElmBool -> "Json.Decode.bool"
  ElmChar -> "elmStreetDecodeChar"
  ElmInt -> "Json.Decode.int"
  ElmFloat -> "Json.Decode.float"
  ElmString -> "Json.Decode.string"
  ElmTime -> "Iso.decoder"
  ElmMaybe t ->
    "maybe"
      <+> typeRefDecoder t
  ElmResult l r ->
    parens "elmStreetDecodeEither"
      <+> typeRefDecoder l
      <+> typeRefDecoder r
  ElmPair a b ->
    parens "elmStreetDecodePair"
      <+> typeRefDecoder a
      <+> typeRefDecoder b
  ElmTriple a b c ->
    parens "elmStreetDecodeTriple"
      <+> typeRefDecoder a
      <+> typeRefDecoder b
      <+> typeRefDecoder c
  ElmList l -> parens "Json.Decode.list" <+> typeRefDecoder l