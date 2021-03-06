module Main exposing (..)

import Array exposing (Array)
import Bool.Extra
import Browser
import Html exposing (Html, button, div, footer, h1, header, input, table, td, text, tr)
import Html.Attributes as Attrs exposing (attribute, class, id, type_)
import Html.Events exposing (onClick, onInput)
import Time


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


encode : Int -> ( Int, Int ) -> Int
encode ncols ( i, j ) =
    i * ncols + j


decode : Int -> Int -> ( Int, Int )
decode ncols index =
    let
        q =
            index // ncols

        r =
            remainderBy ncols index
    in
    ( q, r )


neighbours : Int -> ( Int, Int ) -> List Int
neighbours size ij =
    let
        ( i, j ) =
            ij

        rows =
            [ modBy size (i - 1), i, modBy size (i + 1) ]

        cols =
            [ modBy size (j - 1), j, modBy size (j + 1) ]
    in
    List.concatMap (\row -> List.map (\col -> ( row, col )) cols) rows
        |> List.filter (\n -> not (n == ij))
        |> List.map (encode size)


sumArray : Array Bool -> Int
sumArray vec =
    Array.map
        (\i ->
            case i of
                True ->
                    1

                _ ->
                    0
        )
        vec
        |> Array.foldr (+) 0


gameRules : Bool -> Int -> Bool
gameRules live neigh =
    if live && (neigh == 2 || neigh == 3) then
        True

    else if not live && neigh == 3 then
        True

    else
        False


simulate : Matrix -> Matrix
simulate matrix =
    Matrix
        (Array.indexedMap
            (\idx a ->
                List.sum
                    (List.map
                        (\n ->
                            case Array.get n matrix.array of
                                Just True ->
                                    1

                                _ ->
                                    0
                        )
                        (neighbours matrix.shape (decode matrix.shape idx))
                    )
            )
            matrix.array
            |> Array.indexedMap
                (\idx neigh ->
                    let
                        live =
                            case Array.get idx matrix.array of
                                Just a ->
                                    a

                                _ ->
                                    False
                    in
                    gameRules live neigh
                )
        )
        matrix.shape



---- MODEL ----


type alias Matrix =
    { array : Array Bool
    , shape : Int
    }


type alias Model =
    { running : Bool
    , state : Matrix
    , speed : Int
    }


initialize_model : Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Matrix -> Model
initialize_model active n tickSpeed initialData =
    let
        running =
            case active of
                Just value ->
                    value

                Nothing ->
                    False

        shape =
            case n of
                Just value ->
                    value

                _ ->
                    20

        speed =
            case tickSpeed of
                Just value ->
                    value

                _ ->
                    250

        length =
            shape * shape

        state =
            case initialData of
                Just values ->
                    values

                Nothing ->
                    Matrix (Array.repeat length False) shape
    in
    Model
        running
        state
        speed


init : ( Model, Cmd Msg )
init =
    ( initialize_model Nothing Nothing Nothing Nothing, Cmd.none )



---- UPDATE ----


type Msg
    = SquareClick Int Int
    | Tick Time.Posix
    | Toggle
    | Reset
    | Step
    | Shape String
    | Speed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SquareClick row col ->
            let
                index =
                    encode model.state.shape ( row, col )

                newValue =
                    case Array.get index model.state.array of
                        Just value ->
                            not value

                        Nothing ->
                            False

                updatedArray =
                    Array.set index newValue model.state.array

                state =
                    model.state

                updatedState =
                    { state | array = updatedArray }
            in
            ( { model | state = updatedState }, Cmd.none )

        Toggle ->
            ( { model | running = not model.running }, Cmd.none )

        Tick time ->
            if model.running then
                let
                    nextState =
                        simulate model.state

                    active =
                        if nextState == model.state then
                            False

                        else
                            True
                in
                ( { model | state = nextState, running = active }, Cmd.none )

            else
                ( model, Cmd.none )

        Reset ->
            init

        Step ->
            let
                nextState =
                    simulate model.state
            in
            ( { model | state = nextState, running = False }, Cmd.none )

        Shape value ->
            ( initialize_model
                (Just model.running)
                (String.toInt value)
                (Just model.speed)
                Nothing
            , Cmd.none
            )

        Speed value ->
            ( initialize_model
                (Just model.running)
                (Just model.state.shape)
                (String.toInt value)
                (Just model.state)
            , Cmd.none
            )



---- VIEW ----


squareProperties row col array size =
    let
        r =
            String.fromInt row

        c =
            String.fromInt col

        state =
            case Array.get (encode size ( row, col )) array of
                Just value ->
                    Bool.Extra.toString value

                Nothing ->
                    Bool.Extra.toString False
    in
    [ attribute "state" state
    , attribute "row" r
    , attribute "col" c
    , onClick (SquareClick row col)
    ]


mv state =
    List.map
        (\row ->
            tr [ attribute "data-row" (String.fromInt row) ]
                (List.map (\col -> td (squareProperties row col state.array state.shape) [])
                    (List.range 0 (state.shape - 1))
                )
        )
        (List.range 0 (state.shape - 1))


view : Model -> Document Msg
view model =
    { title = "James' Game of Life"
    , body =
        [ div [ class "main-container" ]
            [ header [ class "header" ] [ text "Life in Elm" ]
            , table [] (mv model.state)
            , div [ class "interface-container" ]
                [ div [ class "button-container" ]
                    [ button [ id "toggle", onClick Toggle ]
                        [ text
                            (if model.running then
                                "Stop"

                             else
                                "Start"
                            )
                        ]
                    , button [ onClick Reset, id "reset" ] [ text "Reset" ]
                    , button [ onClick Step, id "step" ] [ text "Step" ]
                    ]
                , text "Size"
                , input [ class "slider", onInput Shape, id "shape", type_ "range", Attrs.min "15", Attrs.max "60", Attrs.value (String.fromInt model.state.shape) ]
                    []
                , text "Speed"
                , input [ class "slider", onInput Speed, id "speed", type_ "range", Attrs.min "50", Attrs.max "500", Attrs.value (String.fromInt model.speed) ]
                    []
                ]
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (toFloat model.speed) Tick



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
