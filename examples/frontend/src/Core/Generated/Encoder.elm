module Core.Generated.Encoder exposing (..)

import Iso8601 as Iso
import Json.Encode as E exposing (..)

import Core.Generated.ElmStreet exposing (..)
import Core.Generated.Types as T


encodeUser : T.User -> Value
encodeUser x = E.object
    [ ("tag", E.string "User")
    , ("name", E.string x.name)
    , ("age", E.int x.age)
    , ("author", E.bool x.author)
    ]

encodeSortBy : T.SortBy -> Value
encodeSortBy = E.string << T.showSortBy

encodePoint : T.Point -> Value
encodePoint x = E.object
    [ ("tag", E.string "Point")
    , ("id", E.int x.id)
    , ("x", E.int x.x)
    , ("y", E.int x.y)
    , ("z", E.int x.z)
    ]
