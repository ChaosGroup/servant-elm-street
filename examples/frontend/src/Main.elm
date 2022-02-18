module Main exposing (..)

import Browser
import Core.Generated.ElmQueries exposing (..)
import Core.Generated.ElmStreet exposing (..)
import Core.Generated.Types exposing (SortBy, User)
import Debug exposing (toString)
import Html exposing (Html, li, text, ul)
import Http
import Json.Decode exposing (..)
import Maybe exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { postBodySignUpResult : String
    , getSimpleRequestListResult : String
    , getSimpleRequestCustomTypeResult : String
    , postHeadersBasicResult : String
    , postHeadersMultipleResult : String
    , postHeadersCustomTypeResult : String
    }


urlBase : String
urlBase =
    "http://localhost:8081"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { getSimpleRequestListResult = ""
      , getSimpleRequestCustomTypeResult = ""
      , postBodySignUpResult = ""
      , postHeadersBasicResult = ""
      , postHeadersMultipleResult = ""
      , postHeadersCustomTypeResult = ""
      }
    , Cmd.batch
        [ getSimpleRequestList urlBase GotSimpleRequestListResult
        , getSimpleRequestCustomType urlBase GotSimpleRequestCustomTypeResult
        , postBodySignup urlBase GotBodySignUpResult { name = "Maggie", age = 23 }
        , postHeadersBasic urlBase GotHeadersBasicResult { someHeader = Just "Maggie" }
        , postHeadersMultiple urlBase GotHeadersMultipleResult { someHeader1 = Just "He is ", someHeader2 = "23" }
        , postHeadersCustomType urlBase GotHeadersCustomTypeResult { sortBy = Just "Name" }
        ]
    )



-- UPDATE


type Msg
    = GotSimpleRequestListResult (Result Http.Error (List User))
    | GotSimpleRequestCustomTypeResult (Result Http.Error User)
    | GotBodySignUpResult (Result Http.Error User)
    | GotHeadersBasicResult (Result Http.Error String)
    | GotHeadersMultipleResult (Result Http.Error String)
    | GotHeadersCustomTypeResult (Result Http.Error SortBy)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSimpleRequestCustomTypeResult result ->
            ( { model | getSimpleRequestCustomTypeResult = toString result }, Cmd.none )

        GotSimpleRequestListResult result ->
            ( { model | getSimpleRequestListResult = toString result }, Cmd.none )

        GotBodySignUpResult result ->
            ( { model | postBodySignUpResult = toString result }, Cmd.none )

        GotHeadersBasicResult result ->
            ( { model | postHeadersBasicResult = toString result }, Cmd.none )

        GotHeadersMultipleResult result ->
            ( { model | postHeadersMultipleResult = toString result }, Cmd.none )

        GotHeadersCustomTypeResult result ->
            ( { model | postHeadersCustomTypeResult = toString result }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    ul []
        [ li [] [ text <| "/simple/request/list : " ++ model.getSimpleRequestListResult ]
        , li [] [ text <| "/simple/request/customType : " ++ model.getSimpleRequestCustomTypeResult ]
        , li [] [ text <| "/body/signup : " ++ model.postBodySignUpResult ]
        , li [] [ text <| "/headers/basic : " ++ model.postHeadersBasicResult ]
        , li [] [ text <| "/headers/multiple : " ++ model.postHeadersMultipleResult ]
        , li [] [ text <| "/headers/customType : " ++ model.postHeadersCustomTypeResult ]
        ]
