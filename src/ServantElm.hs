{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ServantElm
  ( elmForAPI,
    getReqs,
    generateElmModule,
  )
where

import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Text as T (pack, takeWhile)
import Data.Text.IO as TIO (writeFile)
import Elm (Elm (..))
import Elm.Ast
  ( ElmDefinition,
    ElmPrim (..),
    TypeName (TypeName, unTypeName),
    TypeRef (..),
    definitionToRef,
  )
import Elm.Generate (Settings (..))
import Elm.Print.Common (showDoc)
import Lens.Micro (to, (^.))
import Prettyprinter
  ( Doc,
    braces,
    brackets,
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
    vsep,
    (<+>),
  )
import Servant.Foreign
  ( GenerateList,
    HasForeign (Foreign),
    HasForeignType (..),
    HeaderArg (..),
    PathSegment (unPathSegment),
    Req,
    Segment (unSegment),
    SegmentType (Cap, Static),
    argName,
    argType,
    camelCase,
    headerArg,
    listFromAPI,
    path,
    reqBody,
    reqFuncName,
    reqHeaders,
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

bodyValue :: Doc ann
bodyValue = "bodyValue"

toMsg :: Doc ann
toMsg = "toMsg"

urlBase :: Doc ann
urlBase = "urlBase"

headersValue :: Doc ann
headersValue = "headers"

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

    args = hsep $ [urlBase, toMsg] ++ bodyArg ++ headerArgs
    bodyArg = case requestInfo ^. reqBody of
      Nothing -> []
      Just _ -> [bodyValue]
    headerArgs = [headersValue | not (null $ requestInfo ^. reqHeaders)]

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
    <> line
    <> indent4Spaces "[]"
  where
    urlBuilder :: Doc ann
    urlBuilder = "Url.Builder.crossOrigin" <+> urlBase

    segmentToDoc :: Segment ElmDefinition -> Doc ann
    segmentToDoc s =
      case unSegment s of
        Static sPath ->
          dquotes (pretty (unPathSegment sPath))
        Cap _ ->
          error
            "to implement - for captures, not needed now"

elmTypeRefToDoc :: TypeRef -> Doc ann
elmTypeRefToDoc = \case
  RefPrim elmPrim -> elmPrimToDoc elmPrim
  RefCustom (TypeName typeName) -> pretty typeName

elmTypeParenDoc :: TypeRef -> Doc ann
elmTypeParenDoc = parens . elmTypeRefToDoc

-- taken from elm-street
elmPrimToDoc :: ElmPrim -> Doc ann
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

mkTypeSignature :: Req ElmDefinition -> Doc ann
mkTypeSignature request =
  (hsep . punctuate " ->") ("String" : catMaybes [toMsgType, bodyType, headersRecordType, returnType])
  where
    elmTypeRef :: ElmDefinition -> Doc ann
    elmTypeRef eDef = elmTypeRefToDoc $ definitionToRef eDef
    toMsgType :: Maybe (Doc ann)
    toMsgType =
      fmap mkMsgType $ request ^. reqReturnType
      where
        mkMsgType x = parens $ "Result Http.Error" <+> parens (elmTypeRef x) <+> "-> msg"

    bodyType :: Maybe (Doc ann)
    bodyType = fmap elmTypeRef $ request ^. reqBody

    headerToRecordField :: HeaderArg ElmDefinition -> Doc ann
    headerToRecordField header = headerName <+> ":" <+> headerType
      where
        headerName = header ^. headerArg . argName . to (pretty . unPathSegment)
        headerType = elmTypeRef $ header ^. headerArg . argType

    headersRecordType :: Maybe (Doc ann)
    headersRecordType = if null requestHeaders then Nothing else Just $ braces $ hsep (map headerToRecordField requestHeaders)
      where
        requestHeaders = request ^. reqHeaders

    returnType :: Maybe (Doc ann)
    returnType = pure "Cmd msg"

mkRequest :: Req ElmDefinition -> Doc ann
mkRequest request =
  "Http.request"
    <> line
    <> indent4Spaces
      ( elmRecord
          [ "method ="
              <+> method,
            "headers ="
              <+> headers,
            "url ="
              <+> url,
            "expect ="
              <+> expect,
            "body ="
              <+> body,
            "timeout ="
              <+> "Nothing",
            "tracker ="
              <+> "Nothing"
          ]
      )
    <> line
  where
    method = pretty . pack . show $ request ^. reqMethod

    url =
      mkUrl (request ^. reqUrl . path)

    expect =
      case request ^. reqReturnType of
        Just elmTypeExpr ->
          "Http.expectJson" <+> toMsg <+> (typeRefDecoder . definitionToRef) elmTypeExpr
        Nothing -> error "mkHttpRequest: no reqReturnType?"

    body =
      case request ^. reqBody of
        Just elmTypeExpr ->
          "Http.jsonBody" <+> parens ((typeRefEncoder . definitionToRef) elmTypeExpr <+> bodyValue)
        Nothing ->
          "Http.emptyBody"

    headers =
      if null headerList
        then "[]"
        else
          brackets $
            hsep
              [ renderHeader header
                | header <- headerList
              ]
      where
        headerList = request ^. reqHeaders

renderHeader :: HeaderArg ElmDefinition -> Doc ann
renderHeader headerInfo = "header" <+> "\"" <> headerName <> "\"" <+> parens ("withDefault" <+> "\"\"" <+> "headers." <> headerName)
  where
    headerName = pretty $ headerInfo ^. headerArg . argName . to unPathSegment

-- taken from elm-street
typeRefDecoder :: TypeRef -> Doc ann
typeRefDecoder (RefCustom TypeName {unTypeName}) = "decode" <> pretty (T.takeWhile (/= ' ') unTypeName)
typeRefDecoder (RefPrim elmPrim) = case elmPrim of
  ElmUnit -> parens "JD.map (always ()) (JD.list JD.string)"
  ElmNever -> parens "JD.fail \"Never is not possible\""
  ElmBool -> "JD.bool"
  ElmChar -> "elmStreetDecodeChar"
  ElmInt -> "JD.int"
  ElmFloat -> "JD.float"
  ElmString -> "JD.string"
  ElmTime -> "Iso.decoder"
  ElmMaybe t ->
    parens $
      "JD.maybe"
        <+> typeRefDecoder t
  ElmResult l r ->
    parens $
      "elmStreetDecodeEither"
        <+> typeRefDecoder l
        <+> typeRefDecoder r
  ElmPair a b ->
    parens $
      "elmStreetDecodePair"
        <+> typeRefDecoder a
        <+> typeRefDecoder b
  ElmTriple a b c ->
    parens $
      "elmStreetDecodeTriple"
        <+> typeRefDecoder a
        <+> typeRefDecoder b
        <+> typeRefDecoder c
  ElmList l -> parens $ "JD.list" <+> typeRefDecoder l

-- taken from elm-street
typeRefEncoder :: TypeRef -> Doc ann
typeRefEncoder (RefCustom TypeName {unTypeName}) = "encode" <> pretty (T.takeWhile (/= ' ') unTypeName)
typeRefEncoder (RefPrim elmPrim) = case elmPrim of
  ElmUnit -> "always <| JE.list identity []"
  ElmNever -> "never"
  ElmBool -> "JE.bool"
  ElmChar -> "JE.string << String.fromChar"
  ElmInt -> "JE.int"
  ElmFloat -> "JE.float"
  ElmString -> "JE.string"
  ElmTime -> "Iso.encode"
  ElmMaybe t ->
    "elmStreetEncodeMaybe"
      <+> parens (typeRefEncoder t)
  ElmResult l r ->
    "elmStreetEncodeEither"
      <+> parens (typeRefEncoder l)
      <+> parens (typeRefEncoder r)
  ElmPair a b ->
    "elmStreetEncodePair"
      <+> parens (typeRefEncoder a)
      <+> parens (typeRefEncoder b)
  ElmTriple a b c ->
    "elmStreetEncodeTriple"
      <+> parens (typeRefEncoder a)
      <+> parens (typeRefEncoder b)
      <+> parens (typeRefEncoder c)
  ElmList l -> "JE.list" <+> parens (typeRefEncoder l)

generateElmModule ::
  ( HasForeign LangElm ElmDefinition api,
    GenerateList ElmDefinition (Foreign ElmDefinition api)
  ) =>
  Settings ->
  Proxy api ->
  IO ()
generateElmModule Settings {..} api =
  TIO.writeFile filePath (showDoc moduleFile)
  where
    moduleName :: Doc ann
    moduleName = "Core.Generated.ElmQueries"

    moduleHeader :: Doc ann
    moduleHeader = "module" <+> moduleName <+> "exposing" <+> parens ".."

    queries :: Doc ann
    queries = vsep (elmForAPI api)

    imports :: Doc ann
    imports =
      vsep $
        map
          ("import" <+>)
          [ pretty (List.intercalate "." (settingsModule ++ [settingsDecoderFile])) <+> "exposing" <+> parens "..",
            pretty (List.intercalate "." (settingsModule ++ [settingsEncoderFile])) <+> "exposing" <+> parens "..",
            "Core.Generated.ElmStreet" <+> "exposing" <+> parens "..",
            pretty (List.intercalate "." (settingsModule ++ [settingsTypesFile])) <+> "exposing" <+> parens "..",
            "Http",
            "Json.Decode as JD",
            "Json.Encode as JE",
            "Url.Builder",
            "Maybe" <+> "exposing" <+> parens ".."
          ]

    filePath :: FilePath
    filePath =
      settingsDirectory ++ "/"
        ++ List.intercalate "/" settingsModule
        ++ "/ElmQueries.elm"

    moduleFile :: Doc ann
    moduleFile = moduleHeader <> line <> line <> imports <> line <> line <> queries