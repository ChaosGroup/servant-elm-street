module Core.Generated.ElmQueries exposing (..)

import Core.Generated.Decoder exposing (..)
import Core.Generated.Encoder exposing (..)
import Core.Generated.ElmStreet exposing (..)
import Core.Generated.Types exposing (..)
import Http exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Url.Builder
import Maybe exposing (..)

getSimpleRequestList : String -> (Result Http.Error (List (User)) -> msg) -> Cmd msg
getSimpleRequestList urlBase toMsg =
    Http.request
        { method = "GET"
        , headers = []
        , url = Url.Builder.crossOrigin urlBase
            [ "simple"
            , "request"
            , "list"
            ]
            []
        , expect = Http.expectJson toMsg (JD.list decodeUser)
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }

getSimpleRequestCustomType : String -> (Result Http.Error (User) -> msg) -> Cmd msg
getSimpleRequestCustomType urlBase toMsg =
    Http.request
        { method = "GET"
        , headers = []
        , url = Url.Builder.crossOrigin urlBase
            [ "simple"
            , "request"
            , "customType"
            ]
            []
        , expect = Http.expectJson toMsg decodeUser
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }

postSignup : String -> (Result Http.Error (User) -> msg) -> User -> Cmd msg
postSignup urlBase toMsg bodyValue =
    Http.request
        { method = "POST"
        , headers = []
        , url = Url.Builder.crossOrigin urlBase
            [ "signup"
            ]
            []
        , expect = Http.expectJson toMsg decodeUser
        , body = Http.jsonBody (encodeUser bodyValue)
        , timeout = Nothing
        , tracker = Nothing
        }

postHeadersBasic : String -> (Result Http.Error (String) -> msg) -> {someHeader : String} -> Cmd msg
postHeadersBasic urlBase toMsg headers =
    Http.request
        { method = "POST"
        , headers = [header "someHeader" headers.someHeader]
        , url = Url.Builder.crossOrigin urlBase
            [ "headers"
            , "basic"
            ]
            []
        , expect = Http.expectJson toMsg JD.string
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }

postHeadersMultiple : String -> (Result Http.Error (String) -> msg) -> {someHeader1 : String, someHeader2 : String} -> Cmd msg
postHeadersMultiple urlBase toMsg headers =
    Http.request
        { method = "POST"
        , headers = [header "someHeader1" headers.someHeader1, header "someHeader2" headers.someHeader2]
        , url = Url.Builder.crossOrigin urlBase
            [ "headers"
            , "multiple"
            ]
            []
        , expect = Http.expectJson toMsg JD.string
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }

postHeadersCustomType : String -> (Result Http.Error (SortBy) -> msg) -> {sortBy : String} -> Cmd msg
postHeadersCustomType urlBase toMsg headers =
    Http.request
        { method = "POST"
        , headers = [header "sortBy" headers.sortBy]
        , url = Url.Builder.crossOrigin urlBase
            [ "headers"
            , "customType"
            ]
            []
        , expect = Http.expectJson toMsg decodeSortBy
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }
