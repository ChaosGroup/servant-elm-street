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
  )
where

import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Text as T (pack, takeWhile)
import Elm (Elm (..))
import Elm.Ast
  ( ElmDefinition,
    ElmPrim (..),
    TypeName (TypeName, unTypeName),
    TypeRef (..),
    definitionToRef,
  )
import Lens.Micro ((^.))
import Network.Wai ()
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
  map endpointInfoToElmQuery $
    listFromAPI (Proxy :: Proxy LangElm) (Proxy :: Proxy ElmDefinition) api

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
    urlBuilder = "Url.Builder.crossOrigin urlBase" :: Doc ann

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
  (hsep . punctuate "->") ("String" : catMaybes [toMsgType, returnType])
  where
    elmTypeRef :: ElmDefinition -> Doc ann
    elmTypeRef eDef = elmTypeRefToDoc (definitionToRef eDef)
    toMsgType :: Maybe (Doc ann)
    toMsgType =
      fmap mkMsgType $ request ^. reqReturnType
      where
        mkMsgType x = "(Result Http.Error " <+> parens (elmTypeRef x) <+> "-> msg)"

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
          "Http.expectJson toMsg" <+> renderDecoderName elmTypeExpr
        Nothing -> error "mkHttpRequest: no reqReturnType?"

renderOnlyDecoderName :: ElmDefinition -> Doc
renderOnlyDecoderName elmTypeExpr = case T.words $ prettyShowDecoder elmTypeExpr of
  [] -> emptyDoc
  x : _ -> pretty x

renderDecoderName :: ElmDefinition -> Doc
renderDecoderName elmTypeExpr =
  case elmTypeExpr of
    DefAlias _ -> renderOnlyDecoderName elmTypeExpr
    DefType _ -> renderOnlyDecoderName elmTypeExpr
    DefPrim ElmBool -> "Json.Decode.bool"
    DefPrim ElmInt -> "Json.Decode.int"
    DefPrim ElmChar -> "elmStreetDecodeChar"
    DefPrim ElmFloat -> "Json.Decode.float"
    DefPrim ElmString -> "Json.Decode.string"
    DefPrim (ElmMaybe tRef) -> parens "Json.Decode.maybe" <+> renderDecoderName -- need to have a renderName for typerefs
    DefPrim (ElmResult fst snd) -> undefined
    DefPrim (ElmPair fst snd) -> parens "elmStreetDecodePair " <+> fst <+> " " <+> snd -- need to have a renderName for typerefs
    DefPrim (ElmTriple fst snd trd) -> parens "elmStreetDecodeTriple " <+> fst <+> " " <+> snd <+> " " <+> trd -- need to have a renderName for typerefs
    DefPrim (ElmList tRef) -> parens "Json.Decode.list " <+> elmTypeRefToDoc tRef
    DefPrim _ -> undefined