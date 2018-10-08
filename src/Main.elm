module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, src, type_, value)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type alias Player =
    { name : String, order : Int }


type alias Model =
    { players : List Player
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [ { name = "1", order = 0 }, { name = "2", order = 0 } ], Cmd.none )



---- UPDATE ----


type Msg
    = Shuffle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shuffle ->
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
            [ input [ type_ "button", value "Shuffle", class "bt shuffle-bt", onClick Shuffle ] [] ]
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
