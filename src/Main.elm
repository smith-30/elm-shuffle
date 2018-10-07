module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, src, type_, value)



---- MODEL ----


type alias Player =
    { name : String }


type alias Model =
    { players : List Player
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [ { name = "1" }, { name = "2" } ], Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "grid-container" ]
        [ div [ class "name" ]
            [ input [ type_ "text", value "test" ] [] ]
        , div
            [ class "bt-add" ]
            [ input [ type_ "button", value "Add", class "bt add-bt" ] [] ]
        , div
            [ class "shuffle" ]
            [ input [ type_ "button", value "Shuffle", class "bt shuffle-bt" ] [] ]
        , div
            [ class "name-list" ]
            (descPlayers model.players)
        ]


descPlayers : List Player -> List (Html Msg)
descPlayers args =
    List.map (\p -> div [ class "n" ] [ text p.name ]) args


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
