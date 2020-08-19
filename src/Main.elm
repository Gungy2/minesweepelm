module Main exposing (main)

import Array
import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Matrix
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
    = Bomb
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


type ClickType
    = Try
    | Flag


type alias Model =
    { grid : Matrix.Matrix Cell
    , size : Int
    , clickType : ClickType
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
                , danger = Bomb
                , flagged = False
                }

            else
                { x = x
                , y = y
                , revealed = False
                , danger = Around (countBombs ( x, y ) locs)
                , flagged = False
                }
        )


neighbours : Loc -> List Loc
neighbours ( x, y ) =
    [ ( x - 1, y - 1 )
    , ( x - 1, y )
    , ( x - 1, y + 1 )
    , ( x, y - 1 )
    , ( x, y + 1 )
    , ( x + 1, y - 1 )
    , ( x + 1, y )
    , ( x + 1, y + 1 )
    ]


countBombs : Loc -> List Loc -> Int
countBombs loc bombs =
    loc
        |> neighbours
        |> List.filter (\x -> List.member x bombs)
        |> List.length


init : () -> ( Model, Cmd Msg )
init _ =
    ( { size = 14
      , grid = Matrix.empty
      , clickType = Try
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
    | GenerateBoard (Maybe Int) (List Loc)
    | Switch


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
                        , danger = Bomb
                        , flagged = False
                        }
                        (Matrix.get i j matrix)
            in
            case cell.danger of
                Around 0 ->
                    Maybe.withDefault matrix (modify i j revealCell matrix)
                        |> reveal (locs ++ List.filter (\loc -> not (isRevealed matrix loc) && not (List.member loc locs)) (neighbours ( i, j )))

                Around _ ->
                    Maybe.withDefault matrix (modify i j revealCell matrix)
                        |> reveal locs

                Bomb ->
                    reveal locs matrix

        [] ->
            matrix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked x y ->
            case model.clickType of
                Try ->
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
                        ( { model
                            | grid = reveal [ ( x, y ) ] model.grid
                          }
                        , Cmd.none
                        )

                Flag ->
                    let
                        modified =
                            modify x y (\cell -> { cell | flagged = not cell.flagged }) model.grid
                    in
                    case modified of
                        Nothing ->
                            ( model, Cmd.none )

                        Just newMatrix ->
                            ( { model | grid = newMatrix }, Cmd.none )

        GenerateBoard Nothing locs ->
            ( { model | grid = generateGrid 14 locs, size = 14 }
            , Cmd.none
            )

        GenerateBoard (Just _) locs ->
            ( { model | grid = generateGrid 14 locs, size = 14 }
            , Cmd.none
            )

        Switch ->
            case model.clickType of
                Try ->
                    ( { model | clickType = Flag }, Cmd.none )

                Flag ->
                    ( { model | clickType = Try }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ h1 [] [ text "Minesweeper" ]
        , viewGrid model.grid
        , button [ onClick Switch, id "switch-button" ] [ text "Switch" ]
        ]


convertCell : Cell -> Html Msg
convertCell { x, y, revealed, danger, flagged } =
    if revealed then
        case danger of
            Bomb ->
                img [ src "img/explosion.png" ] []

            Around 0 ->
                text ""

            Around bombs ->
                text (String.fromInt bombs)

    else if flagged then
        button [ onClick (Clicked x y) ] [ img [ src "img/flag.png" ] [] ]

    else
        button [ onClick (Clicked x y) ] []


viewGrid : Matrix.Matrix Cell -> Html Msg
viewGrid matrix =
    table [ id "grid" ]
        (matrix
            |> Matrix.map (td [] << List.singleton << convertCell)
            |> Matrix.toLists
            |> List.map (tr [])
        )
