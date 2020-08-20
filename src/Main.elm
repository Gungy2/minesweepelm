module Main exposing (main)

import Array
import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Json
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
    , size : Int
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
    ( { size = 14
      , grid = Matrix.empty
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
    = Clicked Int Int
    | Flag Int Int
    | Dig Int Int
    | GenerateBoard (Maybe Int) (List Loc)


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


isDone : Matrix.Matrix Cell -> Bool
isDone matrix =
    let
        isBomb : Cell -> Bool
        isBomb { danger } =
            case danger of
                Bomb _ ->
                    True

                _ ->
                    False
    in
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked x y ->
            -- Attempt to dig
            let
                isFlagged : Bool
                isFlagged =
                    Matrix.get x y model.grid
                        |> Maybe.map .flagged
                        |> Maybe.withDefault False
            in
            if isFlagged then
                ( model, Cmd.none )

            else
                let
                    danger : Danger
                    danger =
                        Matrix.get x y model.grid
                            |> Maybe.map .danger
                            |> Maybe.withDefault (Around 0)
                in
                case danger of
                    Bomb _ ->
                        ( { model
                            | grid =
                                modify x
                                    y
                                    (\cell -> { cell | revealed = True, danger = Bomb True })
                                    model.grid
                                    |> Maybe.withDefault model.grid
                                    |> Matrix.map (\cell -> { cell | revealed = True })
                            , gameState = End False
                          }
                        , Cmd.none
                        )

                    _ ->
                        let
                            revealedGrid : Matrix.Matrix Cell
                            revealedGrid =
                                reveal [ ( x, y ) ] model.grid

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
        Flag x y ->
            let
                modified =
                    modify x y (\cell -> { cell | flagged = not cell.flagged }) model.grid
            in
            case modified of
                Nothing ->
                    ( model, Cmd.none )

                Just newMatrix ->
                    ( { model | grid = newMatrix }, Cmd.none )

        -- Revealing the neighbours (if possible)
        Dig x y ->
            let
                countFlags : Int
                countFlags =
                    neighbours model.size ( x, y )
                        |> List.filterMap (\( i, j ) -> Matrix.get i j model.grid)
                        |> List.map .flagged
                        |> List.filter identity
                        |> List.length

                canDig : Bool
                canDig =
                    Matrix.get x y model.grid
                        |> Maybe.andThen getDanger
                        |> Maybe.map ((==) countFlags)
                        |> Maybe.withDefault False
            in
            if canDig then
                Debug.todo "Implement Digging"

            else
                Debug.todo "No Digging for u Sir"

        GenerateBoard Nothing locs ->
            ( { model | grid = generateGrid 14 locs, size = 14 }
            , Cmd.none
            )

        GenerateBoard (Just _) locs ->
            ( { model | grid = generateGrid 14 locs, size = 14 }
            , Cmd.none
            )



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


onRightClick : msg -> Html.Attribute msg
onRightClick message =
    let
        alwaysPreventDefault : msg -> ( msg, Bool )
        alwaysPreventDefault msg =
            ( msg, True )
    in
    Html.Events.preventDefaultOn "contextmenu" <|
        Json.map alwaysPreventDefault (Json.succeed message)


convertCell : Cell -> Html Msg
convertCell { x, y, revealed, danger, flagged } =
    if revealed then
        case danger of
            Bomb True ->
                img [ src "img/explosion.png" ] []

            Bomb False ->
                img [ src "img/mine.png" ] []

            Around 0 ->
                text ""

            Around bombs ->
                button [ onClick (Dig x y) ] [ text (String.fromInt bombs) ]

    else if flagged then
        button [ onClick (Clicked x y), onRightClick (Flag x y) ] [ img [ src "img/flag.png" ] [] ]

    else
        button [ onClick (Clicked x y), onRightClick (Flag x y) ] []


viewGrid : Matrix.Matrix Cell -> Html Msg
viewGrid matrix =
    table [ id "grid" ]
        (matrix
            |> Matrix.map (td [] << List.singleton << convertCell)
            |> Matrix.toLists
            |> List.map (tr [])
        )
