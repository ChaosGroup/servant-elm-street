module Main exposing (..)

import Browser
import Core.Generated.ElmQueries exposing (getAlbert, getUsers, postSignup, postSth)
import Core.Generated.ElmStreet exposing (..)
import Core.Generated.Types exposing (User)
import Debug exposing (toString)
import Html exposing (Html, li, text, ul)
import Http
import Json.Decode exposing (..)



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
    { postSignUpResult : String
    , getUsersResult : String
    , getAlbertResult : String
    , postSthResult : String
    }


urlBase : String
urlBase =
    "http://localhost:8081"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { postSignUpResult = ""
      , getUsersResult = ""
      , getAlbertResult = ""
      , postSthResult = ""
      }
    , Cmd.batch
        [ postSignup urlBase
            GotUserSignUp
            { name = "Maggie"
            , age = 23
            }
        , getUsers urlBase GotUsers
        , getAlbert urlBase GotUserAlbert
        , postSth urlBase GotUserSth
        ]
    )



-- UPDATE


type Msg
    = GotUsers (Result Http.Error (List User))
    | GotUserSignUp (Result Http.Error User)
    | GotUserAlbert (Result Http.Error User)
    | GotUserSth (Result Http.Error User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUsers result ->
            ( { model | getUsersResult = toString result }, Cmd.none )

        GotUserSignUp result ->
            ( { model | postSignUpResult = toString result }, Cmd.none )

        GotUserAlbert result ->
            ( { model | getAlbertResult = toString result }, Cmd.none )

        GotUserSth result ->
            ( { model | postSthResult = toString result }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    ul []
        [ li [] [ text <| "/albert : " ++ model.getAlbertResult ]
        , li [] [ text <| "/users : " ++ model.getUsersResult ]
        , li [] [ text <| "/signup : " ++ model.postSignUpResult ]
        , li [] [ text <| "/sth : " ++ model.postSthResult ]
        ]
