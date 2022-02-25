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
import Url.Builder exposing (..)



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
    , getQueryParametersSingleResult : String
    , getQueryParametersTwoResult : String
    , getQueryParametersRequiredResult : String
    , getQueryParametersCustomFlagResult : String
    , getQueryParametersListResult : String
    , getQueryParametersMixedResult : String
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
      , getQueryParametersSingleResult = ""
      , getQueryParametersTwoResult = ""
      , getQueryParametersRequiredResult = ""
      , getQueryParametersCustomFlagResult = ""
      , getQueryParametersListResult = ""
      , getQueryParametersMixedResult = ""
      }
    , Cmd.batch
        [ getSimpleRequestList urlBase GotSimpleRequestListResult
        , getSimpleRequestCustomType urlBase GotSimpleRequestCustomTypeResult
        , postBodySignup urlBase GotBodySignUpResult { name = "Maggie", age = 23, author = True }
        , postHeadersBasic urlBase GotHeadersBasicResult { someHeader = Just "Maggie" }
        , postHeadersMultiple urlBase GotHeadersMultipleResult { someHeader1 = Just "He is ", someHeader2 = "23" }
        , postHeadersCustomType urlBase GotHeadersCustomTypeResult { sortBy = Just "Name" }
        , getQueryParametersSingle urlBase GotQueryParametersSingle { name = Just "Abba" }
        , getQueryParametersTwo urlBase GotQueryParametersTwo { name = Just "Mina", age = Just "18" }
        , getQueryParametersRequired urlBase GotQueryParametersRequired { name = "Abba" }
        , getQueryParametersCustomFlag urlBase GotQueryParametersCustomFlag { author = Just "True" }
        , getQueryParametersList urlBase GotQueryParametersList { ages = [ "21", "18" ] }
        , getQueryParametersMixed urlBase GotQueryParametersMixed { age = Just "12", name = Just "Annie", authors = [ "Maggie", "Christine", "Paul" ] }
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
    | GotQueryParametersSingle (Result Http.Error (List User))
    | GotQueryParametersTwo (Result Http.Error (List User))
    | GotQueryParametersRequired (Result Http.Error (List User))
    | GotQueryParametersCustomFlag (Result Http.Error (List User))
    | GotQueryParametersList (Result Http.Error (List Int))
    | GotQueryParametersMixed (Result Http.Error String)


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

        GotQueryParametersSingle result ->
            ( { model | getQueryParametersSingleResult = toString result }, Cmd.none )

        GotQueryParametersTwo result ->
            ( { model | getQueryParametersTwoResult = toString result }, Cmd.none )

        GotQueryParametersRequired result ->
            ( { model | getQueryParametersRequiredResult = toString result }, Cmd.none )

        GotQueryParametersCustomFlag result ->
            ( { model | getQueryParametersCustomFlagResult = toString result }, Cmd.none )

        GotQueryParametersList result ->
            ( { model | getQueryParametersListResult = toString result }, Cmd.none )

        GotQueryParametersMixed result ->
            ( { model | getQueryParametersMixedResult = toString result }, Cmd.none )



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
        , li [] [ text <| "/query/parameters/single : " ++ model.getQueryParametersSingleResult ]
        , li [] [ text <| "/query/parameters/two : " ++ model.getQueryParametersTwoResult ]
        , li [] [ text <| "/query/parameters/required : " ++ model.getQueryParametersRequiredResult ]
        , li [] [ text <| "/query/parameters/custom/flag: " ++ model.getQueryParametersCustomFlagResult ]
        , li [] [ text <| "/query/parameters/list : " ++ model.getQueryParametersListResult ]
        , li [] [ text <| "/query/parameters/mixed : " ++ model.getQueryParametersMixedResult ]
        ]
