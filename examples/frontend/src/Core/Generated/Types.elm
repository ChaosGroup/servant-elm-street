module Core.Generated.Types exposing (..)

import Time exposing (Posix)


type alias User =
    { name : String
    , age : Int
    }

type SortBy
    = Age
    | Name

showSortBy : SortBy -> String
showSortBy x = case x of
    Age -> "Age"
    Name -> "Name"

readSortBy : String -> Maybe SortBy
readSortBy x = case x of
    "Age" -> Just Age
    "Name" -> Just Name
    _ -> Nothing

universeSortBy : List SortBy
universeSortBy = [Age, Name]
