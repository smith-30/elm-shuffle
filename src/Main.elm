module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (class, src, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Json
import Random



---- MODEL ----


type alias Player =
    { name : String, order : Int }


type alias Model =
    { players : List Player
    , playerInputVal : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] "", Cmd.none )



---- UPDATE ----


type Msg
    = Shuffle
    | AddPlayer String
    | ChangePlayerInputVal String
    | Delete Int
    | KeyDown Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shuffle ->
            let
                ps =
                    List.map (\p -> { name = p.name, order = genOrder p.order }) model.players
                        |> List.sortWith sortByOrder
            in
            ( { model | players = ps }, Cmd.none )

        AddPlayer v ->
            ( { model | players = addPlayer v model.players, playerInputVal = "" }, Cmd.none )

        ChangePlayerInputVal v ->
            ( { model | playerInputVal = v }, Cmd.none )

        Delete v ->
            ( { model | players = List.filter (\p -> not (p.order == v)) model.players }, Cmd.none )

        KeyDown key ->
            if key == 13 then
                ( { model | players = addPlayer model.playerInputVal model.players, playerInputVal = "" }, Cmd.none )

            else
                ( model, Cmd.none )


genOrder : Int -> Int
genOrder initSeed =
    Random.step (Random.int 0 99999) (Random.initialSeed initSeed)
        |> (\n -> Tuple.first n)


sortByOrder : Player -> Player -> Basics.Order
sortByOrder p1 p2 =
    compare p1.order p2.order


addPlayer : String -> List Player -> List Player
addPlayer str playerList =
    if not (str == "") && List.length (List.filter (\p -> p.name == str) playerList) == 0 then
        List.append playerList [ { name = str, order = genOrder (List.length playerList) } ]

    else
        playerList



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "grid-container" ]
        [ div [ class "name" ]
            [ input [ type_ "text", value model.playerInputVal, onInput (\v -> ChangePlayerInputVal v), onKeyDown KeyDown ] [] ]
        , div
            [ class "bt-add" ]
            [ input [ type_ "button", value "Add", class "bt add-bt", onClick (AddPlayer model.playerInputVal) ] [] ]
        , div
            [ class "shuffle" ]
            [ input [ type_ "button", value "Shuffle", class "bt shuffle-bt", onClick Shuffle ] [] ]
        , div
            [ class "name-list" ]
            (descPlayers model.players)
        ]


descPlayers : List Player -> List (Html Msg)
descPlayers args =
    List.map
        (\p ->
            div [ class "n" ]
                [ text p.name
                , input [ type_ "button", value "Delete", class "bt delete-bt", onClick (Delete p.order) ] []
                ]
        )
        args


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
