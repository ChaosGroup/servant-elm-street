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
    |> required "author" D.bool

decodeSortBy : Decoder T.SortBy
decodeSortBy = elmStreetDecodeEnum T.readSortBy

decodePoint : Decoder T.Point
decodePoint = D.succeed T.Point
    |> required "id" D.int
    |> required "x" D.int
    |> required "y" D.int
    |> required "z" D.int
