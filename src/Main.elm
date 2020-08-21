module Main exposing (main)

import Array
import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Json
import List
import Matrix
import Maybe
import Random
import Random.Array



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type Danger
    = Bomb Bool
    | Around Int


type alias Loc =
    ( Int, Int )


type alias Cell =
    { x : Int
    , y : Int
    , revealed : Bool
    , danger : Danger
    , flagged : Bool
    }


type GameState
    = End Bool
    | Play


type alias Model =
    { grid : Matrix.Matrix Cell
    , gameState : GameState
    }


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs


generateBombs : Int -> Int -> Random.Generator (List Loc)
generateBombs n range =
    cartesian (List.range 1 range) (List.range 1 range)
        |> Array.fromList
        |> Random.Array.sample
        |> Random.map (Maybe.withDefault ( 1, 1 ))
        |> Random.list n


generateGrid : Int -> List Loc -> Matrix.Matrix Cell
generateGrid size locs =
    Matrix.initialize size
        size
        (\( x, y ) ->
            if List.member ( x, y ) locs then
                { x = x
                , y = y
                , revealed = False
                , danger = Bomb False
                , flagged = False
                }

            else
                { x = x
                , y = y
                , revealed = False
                , danger = Around (countBombs size ( x, y ) locs)
                , flagged = False
                }
        )


neighbours : Int -> Loc -> List Loc
neighbours range ( x, y ) =
    [ ( x - 1, y - 1 )
    , ( x - 1, y )
    , ( x - 1, y + 1 )
    , ( x, y - 1 )
    , ( x, y + 1 )
    , ( x + 1, y - 1 )
    , ( x + 1, y )
    , ( x + 1, y + 1 )
    ]
        |> List.filter (\( i, j ) -> i > 0 && i <= range && j > 0 && j <= range)


countBombs : Int -> Loc -> List Loc -> Int
countBombs range loc bombs =
    loc
        |> neighbours range
        |> List.filter (\x -> List.member x bombs)
        |> List.length


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid = Matrix.empty
      , gameState = Play
      }
    , Random.generate (GenerateBoard Nothing) (generateBombs 14 14)
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = Clicked Cell
    | Flag Cell
    | Dig Cell
    | GenerateBoard (Maybe Int) (List Loc)
    | DoNothing


modify : Int -> Int -> (a -> a) -> Matrix.Matrix a -> Maybe (Matrix.Matrix a)
modify i j mod matrix =
    let
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


isRevealed : Matrix.Matrix Cell -> Loc -> Bool
isRevealed matrix ( i, j ) =
    case Matrix.get i j matrix of
        Just cell ->
            cell.revealed

        Nothing ->
            True


isBomb : Cell -> Bool
isBomb { danger } =
    case danger of
        Bomb _ ->
            True

        _ ->
            False


isDone : Matrix.Matrix Cell -> Bool
isDone matrix =
    matrix
        |> Matrix.toList
        |> List.all (\cell -> isBomb cell || cell.revealed)


reveal : List Loc -> Matrix.Matrix Cell -> Matrix.Matrix Cell
reveal list matrix =
    let
        revealCell : Cell -> Cell
        revealCell c =
            { c | revealed = True }
    in
    case list of
        ( i, j ) :: locs ->
            let
                cell : Cell
                cell =
                    Maybe.withDefault
                        { x = i
                        , y = j
                        , revealed = False
                        , danger = Bomb False
                        , flagged = False
                        }
                        (Matrix.get i j matrix)
            in
            case cell.danger of
                Around 0 ->
                    Maybe.withDefault matrix (modify i j revealCell matrix)
                        |> reveal
                            (locs
                                ++ List.filter
                                    (\loc -> not (isRevealed matrix loc) && not (List.member loc locs))
                                    (neighbours (Matrix.height matrix) ( i, j ))
                            )

                Around _ ->
                    Maybe.withDefault matrix (modify i j revealCell matrix)
                        |> reveal locs

                Bomb _ ->
                    reveal locs matrix

        [] ->
            matrix


getDanger : Cell -> Maybe Int
getDanger { danger } =
    case danger of
        Around x ->
            Just x

        Bomb _ ->
            Nothing


updateDig : Model -> Cell -> Model
updateDig model cell =
    let
        countFlags : Int
        countFlags =
            neighbours (Matrix.height model.grid) ( cell.x, cell.y )
                |> List.filterMap (\( i, j ) -> Matrix.get i j model.grid)
                |> List.map .flagged
                |> List.filter identity
                |> List.length

        canDig : Bool
        canDig =
            cell
                |> getDanger
                |> Maybe.map ((==) countFlags)
                |> Maybe.withDefault False
    in
    if canDig then
        let
            unrevNeigh : List Cell
            unrevNeigh =
                neighbours (Matrix.height model.grid) ( cell.x, cell.y )
                    |> List.filterMap (\( i, j ) -> Matrix.get i j model.grid)
                    |> List.filter (\el -> not (el.revealed || el.flagged))

            nearBombs : List Loc
            nearBombs =
                unrevNeigh
                    |> List.filter isBomb
                    |> List.map (\{ x, y } -> ( x, y ))
        in
        if List.isEmpty nearBombs then
            let
                revealedGrid : Matrix.Matrix Cell
                revealedGrid =
                    reveal
                        (List.map (\{ x, y } -> ( x, y )) unrevNeigh)
                        model.grid

                status : GameState
                status =
                    if isDone revealedGrid then
                        End True

                    else
                        Play
            in
            { grid = revealedGrid
            , gameState = status
            }

        else
            let
                finalGrid : Matrix.Matrix Cell
                finalGrid =
                    List.foldr
                        (\( i, j ) matrix ->
                            Maybe.withDefault
                                model.grid
                                (modify i j (\el -> { el | danger = Bomb True }) matrix)
                        )
                        model.grid
                        nearBombs
                        |> Matrix.map (\el -> { el | revealed = True })
            in
            { grid = finalGrid, gameState = End False }

    else
        model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked cell ->
            if cell.flagged then
                ( model, Cmd.none )

            else
                case cell.danger of
                    Bomb _ ->
                        ( { model
                            | grid =
                                modify cell.x
                                    cell.y
                                    (\el -> { el | revealed = True, danger = Bomb True })
                                    model.grid
                                    |> Maybe.withDefault model.grid
                                    |> Matrix.map (\el -> { el | revealed = True })
                            , gameState = End False
                          }
                        , Cmd.none
                        )

                    _ ->
                        let
                            revealedGrid : Matrix.Matrix Cell
                            revealedGrid =
                                reveal [ ( cell.x, cell.y ) ] model.grid

                            status : GameState
                            status =
                                if isDone revealedGrid then
                                    End True

                                else
                                    Play
                        in
                        ( { model
                            | grid = revealedGrid
                            , gameState = status
                          }
                        , Cmd.none
                        )

        -- Flag the square
        Flag cell ->
            let
                modified =
                    modify cell.x cell.y (\el -> { el | flagged = not cell.flagged }) model.grid
            in
            case modified of
                Nothing ->
                    ( model, Cmd.none )

                Just newMatrix ->
                    ( { model | grid = newMatrix }, Cmd.none )

        -- Revealing the neighbours (if possible)
        Dig cell ->
            ( updateDig model cell, Cmd.none )

        GenerateBoard Nothing locs ->
            ( { model | grid = generateGrid 14 locs }
            , Cmd.none
            )

        GenerateBoard (Just _) locs ->
            ( { model | grid = generateGrid 14 locs }
            , Cmd.none
            )

        DoNothing ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ h1 [] [ text "Minesweeper" ]
        , viewGrid model.grid
        , h2 []
            [ case model.gameState of
                End True ->
                    text "Congrats!!!"

                End False ->
                    text "U SUCC!"

                Play ->
                    text ""
            ]
        ]


preventContextMenu : msg -> Html.Attribute msg
preventContextMenu message =
    let
        alwaysPreventDefault : msg -> ( msg, Bool )
        alwaysPreventDefault msg =
            ( msg, True )
    in
    Html.Events.preventDefaultOn "contextmenu" <|
        Json.map alwaysPreventDefault (Json.succeed message)


onBothClicks : msg -> Html.Attribute msg
onBothClicks message =
    Html.Events.on "mouseup" <|
        (Json.field "buttons" Json.int
            |> Json.andThen
                (\buttons ->
                    if buttons == 2 then
                        Json.succeed message

                    else
                        Json.fail "Not Both Clicks"
                )
        )


onRightClick : msg -> Html.Attribute msg
onRightClick message =
    Html.Events.on "mouseup" <|
        (Json.field "button" Json.int
            |> Json.andThen
                (\button ->
                    if button == 2 then
                        Json.succeed message

                    else
                        Json.fail "Not Right Click"
                )
        )


convertCell : Cell -> Html Msg
convertCell cell =
    if cell.revealed then
        case cell.danger of
            Bomb True ->
                img [ src "img/explosion.png" ] []

            Bomb False ->
                img [ src "img/mine.png" ] []

            Around 0 ->
                text ""

            Around bombs ->
                button [ onBothClicks (Dig cell) ] [ text (String.fromInt bombs) ]

    else
        button [ onClick (Clicked cell), onRightClick (Flag cell) ]
            [ if cell.flagged then
                img [ src "img/flag.png" ] []

              else
                text ""
            ]


viewGrid : Matrix.Matrix Cell -> Html Msg
viewGrid matrix =
    table [ id "grid", preventContextMenu DoNothing ]
        (matrix
            |> Matrix.map (td [] << List.singleton << convertCell)
            |> Matrix.toLists
            |> List.map (tr [])
        )
