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
import Maybe.Extra exposing (..)
import Url.Builder exposing (..)
import List

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

postBodySignup : String -> (Result Http.Error (User) -> msg) -> User -> Cmd msg
postBodySignup urlBase toMsg bodyValue =
    Http.request
        { method = "POST"
        , headers = []
        , url = Url.Builder.crossOrigin urlBase
            [ "body"
            , "signup"
            ]
            []
        , expect = Http.expectJson toMsg decodeUser
        , body = Http.jsonBody (encodeUser bodyValue)
        , timeout = Nothing
        , tracker = Nothing
        }

postHeadersBasic : String -> (Result Http.Error (String) -> msg) -> {someHeader : Maybe String} -> Cmd msg
postHeadersBasic urlBase toMsg headers =
    Http.request
        { method = "POST"
        , headers = values [Maybe.map (header "someHeader") headers.someHeader]
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

postHeadersMultiple : String -> (Result Http.Error (String) -> msg) -> {someHeader1 : Maybe String, someHeader2 : String} -> Cmd msg
postHeadersMultiple urlBase toMsg headers =
    Http.request
        { method = "POST"
        , headers = values [Maybe.map (header "someHeader1") headers.someHeader1, Just <| header "someHeader2" headers.someHeader2]
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

postHeadersCustomType : String -> (Result Http.Error (SortBy) -> msg) -> {sortBy : Maybe String} -> Cmd msg
postHeadersCustomType urlBase toMsg headers =
    Http.request
        { method = "POST"
        , headers = values [Maybe.map (header "sortBy") headers.sortBy]
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

getQueryParametersSingle : String -> (Result Http.Error (List (User)) -> msg) -> {name : Maybe String} -> Cmd msg
getQueryParametersSingle urlBase toMsg queryParameters =
    Http.request
        { method = "GET"
        , headers = []
        , url = Url.Builder.crossOrigin urlBase
            [ "query"
            , "parameters"
            , "single"
            ]
            (List.filterMap identity <| List.foldl (++) [] [[Maybe.map (string "name") queryParameters.name]])
        , expect = Http.expectJson toMsg (JD.list decodeUser)
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }

getQueryParametersTwo : String -> (Result Http.Error (List (User)) -> msg) -> {name : Maybe String, age : Maybe String} -> Cmd msg
getQueryParametersTwo urlBase toMsg queryParameters =
    Http.request
        { method = "GET"
        , headers = []
        , url = Url.Builder.crossOrigin urlBase
            [ "query"
            , "parameters"
            , "two"
            ]
            (List.filterMap identity <| List.foldl (++) [] [[Maybe.map (string "name") queryParameters.name], [Maybe.map (string "age") queryParameters.age]])
        , expect = Http.expectJson toMsg (JD.list decodeUser)
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }

getQueryParametersRequired : String -> (Result Http.Error (List (User)) -> msg) -> {name : String} -> Cmd msg
getQueryParametersRequired urlBase toMsg queryParameters =
    Http.request
        { method = "GET"
        , headers = []
        , url = Url.Builder.crossOrigin urlBase
            [ "query"
            , "parameters"
            , "required"
            ]
            (List.filterMap identity <| List.foldl (++) [] [[Just <| string "name" queryParameters.name]])
        , expect = Http.expectJson toMsg (JD.list decodeUser)
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }

getQueryParametersCustomFlag : String -> (Result Http.Error (List (User)) -> msg) -> {author : Maybe String} -> Cmd msg
getQueryParametersCustomFlag urlBase toMsg queryParameters =
    Http.request
        { method = "GET"
        , headers = []
        , url = Url.Builder.crossOrigin urlBase
            [ "query"
            , "parameters"
            , "custom"
            , "flag"
            ]
            (List.filterMap identity <| List.foldl (++) [] [[Maybe.map (string "author") queryParameters.author]])
        , expect = Http.expectJson toMsg (JD.list decodeUser)
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }

getQueryParametersList : String -> (Result Http.Error (List (Int)) -> msg) -> {ages : (List String)} -> Cmd msg
getQueryParametersList urlBase toMsg queryParameters =
    Http.request
        { method = "GET"
        , headers = []
        , url = Url.Builder.crossOrigin urlBase
            [ "query"
            , "parameters"
            , "list"
            ]
            (List.filterMap identity <| List.foldl (++) [] [(List.map (\listValue -> Just <| string "ages" listValue) queryParameters.ages)])
        , expect = Http.expectJson toMsg (JD.list JD.int)
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }

getQueryParametersMixed : String -> (Result Http.Error (String) -> msg) -> {age : Maybe String, name : Maybe String, authors : (List String)} -> Cmd msg
getQueryParametersMixed urlBase toMsg queryParameters =
    Http.request
        { method = "GET"
        , headers = []
        , url = Url.Builder.crossOrigin urlBase
            [ "query"
            , "parameters"
            , "mixed"
            ]
            (List.filterMap identity <| List.foldl (++) [] [[Maybe.map (string "age") queryParameters.age], [Maybe.map (string "name") queryParameters.name], (List.map (\listValue -> Just <| string "authors" listValue) queryParameters.authors)])
        , expect = Http.expectJson toMsg JD.string
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }
