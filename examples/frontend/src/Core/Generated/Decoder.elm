module Core.Generated.Decoder exposing (..)

import Iso8601 as Iso
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D exposing (required)

import Core.Generated.ElmStreet exposing (..)
import Core.Generated.Types as T


decodeUser : Decoder T.User
decodeUser = D.succeed T.User
    |> required "name" D.string
    |> required "age" D.int

decodeSortBy : Decoder T.SortBy
decodeSortBy = elmStreetDecodeEnum T.readSortBy
