module Main exposing (..)

import Html exposing (programWithFlags, Html, div, text)
import Html.Events exposing (onClick)
import Html.CssHelpers
import Rocket exposing ((=>))
import Matrix exposing (Matrix)
import Matrix.Extra
import Random.Pcg as Random
import Array exposing (Array)
import Styles


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- model


type alias Cell =
    { discovered : Bool
    , mine : Bool
    }


initCell : Bool -> Cell
initCell mine =
    { discovered = False
    , mine = mine
    }


type GamePhase
    = Playing
    | GameOver


type alias Model =
    { phase : GamePhase
    , field : Matrix Cell
    , seed : Random.Seed
    }


neighbouringMines : Int -> Int -> Matrix Cell -> Int
neighbouringMines x y field =
    Matrix.Extra.neighbours x y field
        |> List.filter isMine
        |> List.length


( fieldWidth, fieldHeight ) =
    ( 50, 40 )


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
    in
        { phase = Playing
        , field = field
        , seed = newSeed
        }


type alias Flags =
    Int


init : Flags -> ( Model, List (Cmd Msg) )
init flags =
    randomModel flags => []



-- update


type Msg
    = ClickCell Int Int


update : Msg -> Model -> ( Model, List (Cmd Msg) )
update action model =
    case action of
        ClickCell x y ->
            handleClick x y model
                => []


showCell : Cell -> Cell
showCell cell =
    { cell
        | discovered = True
    }


isDiscovered : Cell -> Bool
isDiscovered cell =
    cell.discovered


isMine : Cell -> Bool
isMine cell =
    cell.mine


handleClick : Int -> Int -> Model -> Model
handleClick x y model =
    case Matrix.get x y model.field of
        Nothing ->
            model

        Just aCell ->
            if aCell.mine then
                { model
                    | field = Matrix.update x y showCell model.field
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
                    if
                        neighbouringMines x y field
                            > 0
                    then
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
                    not cell.discovered
                        && not cell.mine
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
    div [] [ renderField model.field ]


renderField : Matrix Cell -> Html Msg
renderField field =
    List.range 0 ((Matrix.height field) - 1)
        |> List.map
            (\idx ->
                Matrix.getRow idx field
                    |> Maybe.withDefault Array.empty
                    |> renderRow field idx
            )
        |> div []


renderRow : Matrix Cell -> Int -> Array Cell -> Html Msg
renderRow field row data =
    Array.toList data
        |> List.indexedMap (renderCell field row)
        |> div [ class [ Styles.Row ] ]


renderCell : Matrix Cell -> Int -> Int -> Cell -> Html Msg
renderCell field y x cell =
    case ( cell.discovered, cell.mine ) of
        ( True, True ) ->
            renderMine

        ( True, False ) ->
            renderNumbered field x y

        ( False, _ ) ->
            renderPotential x y


renderMine : Html Msg
renderMine =
    div [ class [ Styles.Cell, Styles.Mine ] ] []


renderNumbered : Matrix Cell -> Int -> Int -> Html Msg
renderNumbered field x y =
    let
        number : String
        number =
            case neighbouringMines x y field of
                0 ->
                    ""

                n ->
                    toString n
    in
        div [ class [ Styles.Cell, Styles.Discovered ] ]
            [ text number ]


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
