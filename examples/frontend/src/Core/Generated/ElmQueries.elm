module Core.Generated.ElmQueries exposing (..)

import Core.Generated.Decoder exposing (..)
import Core.Generated.Encoder exposing (..)
import Core.Generated.ElmStreet exposing (..)
import Core.Generated.Types exposing (..)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Url.Builder

getUsers : String -> (Result Http.Error (List (User)) -> msg) -> Cmd msg
getUsers urlBase toMsg =
    Http.request
        { method = "GET"
        , headers = []
        , url = Url.Builder.crossOrigin urlBase
            [ "users"
            ]
            []
        , expect = Http.expectJson toMsg (JD.list decodeUser)
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }

getAlbert : String -> (Result Http.Error (User) -> msg) -> Cmd msg
getAlbert urlBase toMsg =
    Http.request
        { method = "GET"
        , headers = []
        , url = Url.Builder.crossOrigin urlBase
            [ "albert"
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

postSth : String -> (Result Http.Error (User) -> msg) -> Cmd msg
postSth urlBase toMsg =
    Http.request
        { method = "POST"
        , headers = []
        , url = Url.Builder.crossOrigin urlBase
            [ "sth"
            ]
            []
        , expect = Http.expectJson toMsg decodeUser
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }
