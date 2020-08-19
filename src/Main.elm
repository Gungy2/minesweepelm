module Main exposing (main)

import Array
import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Matrix



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Cell =
    { x : Int
    , y : Int
    , revealed : Bool
    , bomb : Bool
    }


type alias Model =
    { grid : Matrix.Matrix Cell
    }


init : Model
init =
    { grid =
        Matrix.initialize
            14
            14
            (\( x, y ) ->
                let
                    template : Cell
                    template =
                        { revealed = False, bomb = False, x = x, y = y }
                in
                if Basics.modBy 5 (x + y) == 0 then
                    { template | bomb = True }

                else
                    template
            )
    }



-- UPDATE


type Msg
    = Reveal Int Int


modify : Int -> Int -> (a -> a) -> Matrix.Matrix a -> Maybe (Matrix.Matrix a)
modify i j mod matrix =
    let
        log =
            Debug.log "Dimensions" ( i, j )

        width =
            Matrix.width matrix

        height =
            Matrix.height matrix

        array =
            matrix
                |> Matrix.toList
                |> Array.fromList

        maybeOldElement =
            Array.get ((i - 1) * width + j - 1) array

        newArray =
            case maybeOldElement of
                Just oldElement ->
                    Array.set ((i - 1) * width + j - 1) (mod oldElement) array

                Nothing ->
                    array
    in
    if i > height || i <= 0 || j > width || j <= 0 then
        Maybe.Nothing

    else
        newArray
            |> Array.toList
            |> Matrix.fromList height width


reveal : Int -> Int -> Matrix.Matrix Cell -> Matrix.Matrix Cell
reveal i j matrix =
    let
        revealCell : Cell -> Cell
        revealCell c =
            { c | revealed = True }
    in
    Maybe.withDefault matrix (modify i j revealCell matrix)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reveal x y ->
            { grid = reveal x y model.grid
            }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Minesweeper" ]
        , viewGrid model.grid
        ]


convertCell : Cell -> Html Msg
convertCell { x, y, revealed, bomb } =
    if revealed then
        if bomb then
            td [] [ img [ src "img/flag.png" ] [] ]

        else
            td [] [ text "Revealed" ]

    else
        button [ onClick (Reveal x y) ] [ text "O" ]


viewGrid : Matrix.Matrix Cell -> Html Msg
viewGrid matrix =
    table [ id "grid" ]
        (matrix
            |> Matrix.map (td [] << List.singleton << convertCell)
            |> Matrix.toLists
            |> List.map (tr [])
        )
