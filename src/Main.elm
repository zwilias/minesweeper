module Main exposing (..)

import Html exposing (programWithFlags, Html, div, text, h1)
import Html.Lazy
import Html.Events exposing (onClick, onDoubleClick)
import Html.CssHelpers
import Rocket exposing ((=>))
import Matrix exposing (Matrix)
import Matrix.Extra
import Random.Pcg as Random
import Array exposing (Array)
import Styles
import Keyboard


-- app


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init >> Rocket.batchInit
        , update = update >> Rocket.batchUpdate
        , subscriptions = subscriptions
        , view = view
        }



-- subscriptions


shiftKey : Keyboard.KeyCode
shiftKey =
    16


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs
            (\x ->
                if x == shiftKey then
                    PressShift
                else
                    NoOp
            )
        , Keyboard.ups
            (\x ->
                if x == shiftKey then
                    ReleaseShift
                else
                    NoOp
            )
        ]



-- model


type CellState
    = Potential
    | Flagged
    | Discovered


type alias Cell =
    { state : CellState
    , mine : Bool
    , neighbours : Int
    }


initCell : Bool -> Cell
initCell mine =
    { state = Potential
    , mine = mine
    , neighbours = 0
    }


type GamePhase
    = Playing
    | GameOver


type alias Model =
    { phase : GamePhase
    , field : Matrix Cell
    , seed : Random.Seed
    , shiftDown : Bool
    , flagsLeft : Int
    }


countNeighbouringMines : Int -> Int -> Matrix Cell -> Int
countNeighbouringMines x y field =
    Matrix.Extra.neighbours x y field
        |> List.filter isMine
        |> List.length


( fieldWidth, fieldHeight ) =
    ( 20, 20 )


randomModel : Int -> Model
randomModel flags =
    let
        seed : Random.Seed
        seed =
            Random.initialSeed flags

        randomBools : Int -> ( List Bool, Random.Seed )
        randomBools chance =
            Random.step (Random.list (fieldWidth * fieldHeight) <| Random.oneIn chance) seed

        ( bools, newSeed ) =
            randomBools 9

        field : Matrix Cell
        field =
            bools
                |> List.map initCell
                |> Array.fromList
                |> Matrix ( fieldWidth, fieldHeight )

        addNeighbourCounts : Matrix Cell -> Matrix Cell
        addNeighbourCounts field =
            Matrix.indexedMap
                (\x y cell ->
                    { cell
                        | neighbours = countNeighbouringMines x y field
                    }
                )
                field
    in
        { phase = Playing
        , field = field |> addNeighbourCounts
        , seed = newSeed
        , shiftDown = False
        , flagsLeft = field |> countFlagsLeft
        }


type alias Flags =
    Int


init : Flags -> ( Model, List (Cmd Msg) )
init flags =
    randomModel flags => []


countFlagsLeft : Matrix Cell -> Int
countFlagsLeft cellMatrix =
    let
        ( flags, bombs ) =
            cellMatrix.data
                |> Array.foldl
                    (\cell ( flags, bombs ) ->
                        let
                            nFlags =
                                if isFlag cell then
                                    flags + 1
                                else
                                    flags

                            nBombs =
                                if isMine cell then
                                    bombs + 1
                                else
                                    bombs
                        in
                            ( nFlags, nBombs )
                    )
                    ( 0, 0 )
    in
        bombs - flags



-- update


type Msg
    = ClickCell Int Int
    | NoOp
    | PressShift
    | ReleaseShift
    | DoubleClickCell Int Int Cell


update : Msg -> Model -> ( Model, List (Cmd Msg) )
update action model =
    case action of
        ClickCell x y ->
            case model.phase of
                Playing ->
                    if model.shiftDown then
                        toggleFlag x y model
                            => []
                    else
                        handleClick x y model
                            => []

                _ ->
                    model => []

        PressShift ->
            { model
                | shiftDown = True
            }
                => []

        ReleaseShift ->
            { model
                | shiftDown = False
            }
                => []

        DoubleClickCell x y cell ->
            case model.phase of
                Playing ->
                    -- handle that
                    model
                        |> floodFromCell x y cell
                        => []

                _ ->
                    model => []

        NoOp ->
            model => []


countNeighbouringFlags : Int -> Int -> Matrix Cell -> Int
countNeighbouringFlags x y field =
    Matrix.Extra.neighbours x y field
        |> List.filter isFlag
        |> List.length


hasWronglyFlaggedNeighbours : Int -> Int -> Matrix Cell -> Bool
hasWronglyFlaggedNeighbours x y field =
    Matrix.Extra.neighbours x y field
        |> List.any (\cell -> xor (isFlag cell) (isMine cell))


floodFromCell : Int -> Int -> Cell -> Model -> Model
floodFromCell x y cell model =
    if
        (cell.neighbours == 0)
            || (cell.neighbours /= countNeighbouringFlags x y model.field)
    then
        model
    else if hasWronglyFlaggedNeighbours x y model.field then
        { model
            | field = model.field |> revealMines
            , phase = GameOver
        }
    else
        let
            fieldsToDiscover : List ( Int, Int )
            fieldsToDiscover =
                Matrix.Extra.indexedNeighbours x y model.field
                    |> List.filter
                        (\( ( x, y ), cell ) ->
                            isPotential cell
                        )
                    |> List.map Tuple.first

            updatedField : Matrix Cell
            updatedField =
                fieldsToDiscover
                    |> List.foldl
                        (\( x, y ) field ->
                            discover x y field
                        )
                        model.field
        in
            { model
                | field = updatedField
            }


showCell : Cell -> Cell
showCell cell =
    { cell
        | state = Discovered
    }


isPotential : Cell -> Bool
isPotential cell =
    cell.state == Potential


isFlag : Cell -> Bool
isFlag cell =
    cell.state == Flagged


isMine : Cell -> Bool
isMine cell =
    cell.mine


toggleFlagState : Cell -> Cell
toggleFlagState cell =
    let
        newState =
            if isPotential cell then
                Flagged
            else
                Potential
    in
        { cell
            | state = newState
        }


toggleFlag : Int -> Int -> Model -> Model
toggleFlag x y model =
    let
        newField =
            Matrix.update x y toggleFlagState model.field
    in
        { model
            | field = newField
            , flagsLeft = countFlagsLeft newField
        }


revealMines : Matrix Cell -> Matrix Cell
revealMines =
    Matrix.map
        (\cell ->
            if isMine cell then
                showCell cell
            else
                cell
        )


handleClick : Int -> Int -> Model -> Model
handleClick x y model =
    case Matrix.get x y model.field of
        Nothing ->
            model

        Just aCell ->
            if aCell.mine then
                { model
                    | field = model.field |> revealMines
                    , phase = GameOver
                }
            else
                { model
                    | field = discover x y model.field
                }


discover : Int -> Int -> Matrix Cell -> Matrix Cell
discover x y field =
    let
        neighbours : Matrix Cell -> List ( ( Int, Int ), Cell )
        neighbours field =
            case Matrix.get x y field of
                Nothing ->
                    []

                Just cell ->
                    if cell.neighbours > 0 then
                        []
                    else
                        Matrix.Extra.indexedNeighbours x y field

        updatedField =
            Matrix.update x y showCell field
    in
        updatedField
            |> neighbours
            |> List.filter
                (\( ( cX, cY ), cell ) ->
                    isPotential cell
                        && (not <| isMine cell)
                )
            |> List.foldl
                (\( ( cX, cY ), cell ) field ->
                    discover cX cY field
                )
                updatedField



-- view


{ id, class, classList } =
    Html.CssHelpers.withNamespace "sweeper"


view : Model -> Html Msg
view model =
    div [ class [ Styles.Wrapper ] ]
        [ h1 [] [ text <| toString model.flagsLeft ]
        , renderField model.field
        ]


renderField : Matrix Cell -> Html Msg
renderField field =
    List.range 0 ((Matrix.height field) - 1)
        |> List.map
            (\idx ->
                Matrix.getRow idx field
                    |> Maybe.withDefault Array.empty
                    |> Html.Lazy.lazy2 renderRow idx
            )
        |> div []


renderRow : Int -> Array Cell -> Html Msg
renderRow row data =
    Array.toList data
        |> List.indexedMap (Html.Lazy.lazy3 renderCell row)
        |> div [ class [ Styles.Row ] ]


renderCell : Int -> Int -> Cell -> Html Msg
renderCell y x cell =
    case ( cell.state, cell.mine ) of
        ( Discovered, True ) ->
            renderMine

        ( Discovered, False ) ->
            renderNumbered cell x y

        ( Flagged, _ ) ->
            renderFlagged x y

        ( Potential, _ ) ->
            renderPotential x y


renderMine : Html Msg
renderMine =
    div [ class [ Styles.Cell, Styles.Mine ] ] []


renderNumbered : Cell -> Int -> Int -> Html Msg
renderNumbered cell x y =
    let
        number : String
        number =
            case cell.neighbours of
                0 ->
                    ""

                n ->
                    toString n
    in
        div
            [ class [ Styles.Cell, Styles.Discovered ]
            , onDoubleClick <| DoubleClickCell x y cell
            ]
            [ text number ]


renderFlagged : Int -> Int -> Html Msg
renderFlagged x y =
    div
        [ class
            [ Styles.Cell
            , Styles.Flagged
            ]
        , onClick (ClickCell x y)
        ]
        [ text "F" ]


renderPotential : Int -> Int -> Html Msg
renderPotential x y =
    div
        [ class
            [ Styles.Cell
            , Styles.Potential
            ]
        , onClick (ClickCell x y)
        ]
        []
