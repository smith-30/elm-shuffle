module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, src, type_, value)
import Html.Events exposing (onClick, onInput)
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
            let
                ps =
                    if not (v == "") && List.length (List.filter (\p -> p.name == v) model.players) == 0 then
                        List.append model.players [ { name = v, order = genOrder (List.length model.players) } ]

                    else
                        model.players
            in
            ( { model | players = ps, playerInputVal = "" }, Cmd.none )

        ChangePlayerInputVal v ->
            ( { model | playerInputVal = v }, Cmd.none )


genOrder : Int -> Int
genOrder initSeed =
    Random.step (Random.int 0 99999) (Random.initialSeed initSeed)
        |> (\n -> Tuple.first n)


sortByOrder : Player -> Player -> Basics.Order
sortByOrder p1 p2 =
    compare p1.order p2.order


emptyNameFilter : Player -> Bool
emptyNameFilter p =
    if p.name == "" then
        False

    else
        True


duplicateNameFilter : Player -> String -> Bool
duplicateNameFilter p v =
    if p.name == v then
        True

    else
        False



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "grid-container" ]
        [ div [ class "name" ]
            [ input [ type_ "text", value model.playerInputVal, onInput (\v -> ChangePlayerInputVal v) ] [] ]
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
