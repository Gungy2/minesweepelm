module Main exposing (main)

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Json
import List
import List.Extra
import Matrix
import Maybe
import Random
import Random.Array
import Time



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
    | Around Int Bool


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
    | Between


type alias Model =
    { grid : Matrix.Matrix Cell
    , gameState : GameState
    , time : Time.Posix
    , size : Int
    , flags : Int
    , bombs : Int
    }


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs


generateBombs : Int -> Int -> Loc -> Random.Generator (List Loc)
generateBombs n range rem =
    cartesian (List.range 1 range) (List.range 1 range)
        |> List.Extra.remove rem
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
                , danger = Around (countBombs size ( x, y ) locs) False
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
      , gameState = Between
      , time = Time.millisToPosix 0
      , size = 14
      , flags = 0
      , bombs = 0
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        Play ->
            Time.every 100 Tick

        _ ->
            Sub.none



-- UPDATE


type Msg
    = Clicked Cell
    | Flag Cell
    | Dig Cell
    | GenerateBoard Loc (List Loc)
    | Tick Time.Posix
    | ChangeSize Int
    | Start Loc
    | StartAgain
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
            if cell.flagged then
                reveal locs matrix

            else
                case cell.danger of
                    Around 0 _ ->
                        Maybe.withDefault matrix (modify i j revealCell matrix)
                            |> reveal
                                (locs
                                    ++ List.filter
                                        (\loc -> not (isRevealed matrix loc) && not (List.member loc locs))
                                        (neighbours (Matrix.height matrix) ( i, j ))
                                )

                    Around _ _ ->
                        Maybe.withDefault matrix (modify i j revealCell matrix)
                            |> reveal locs

                    Bomb _ ->
                        reveal locs matrix

        [] ->
            matrix


getDanger : Cell -> Maybe Int
getDanger { danger } =
    case danger of
        Around x _ ->
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
            case status of
                Play ->
                    { model
                        | grid = revealedGrid
                        , gameState = Play
                    }

                _ ->
                    { model
                        | grid = Matrix.map (\el -> { el | revealed = True }) revealedGrid
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
                        |> Matrix.map
                            (\el ->
                                { el
                                    | revealed = True
                                    , danger =
                                        case el.danger of
                                            Around x _ ->
                                                Around x el.flagged

                                            anything ->
                                                anything
                                }
                            )
            in
            { model | grid = finalGrid, gameState = End False }

    else
        model


updateReveal : Model -> Cell -> Model
updateReveal model cell =
    if cell.flagged then
        model

    else
        case cell.danger of
            Bomb _ ->
                { model
                    | grid =
                        modify cell.x
                            cell.y
                            (\el -> { el | revealed = True, danger = Bomb True })
                            model.grid
                            |> Maybe.withDefault model.grid
                            |> Matrix.map
                                (\el ->
                                    { el
                                        | revealed = True
                                        , danger =
                                            case el.danger of
                                                Around x _ ->
                                                    Around x el.flagged

                                                anything ->
                                                    anything
                                    }
                                )
                    , gameState = End False
                }

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
                case status of
                    Play ->
                        { model | grid = revealedGrid, gameState = Play }

                    _ ->
                        { model | grid = Matrix.map (\el -> { el | revealed = True }) revealedGrid, gameState = status }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked cell ->
            ( updateReveal model cell, Cmd.none )

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
                    let
                        flagMod : Int
                        flagMod =
                            if cell.flagged then
                                -1

                            else
                                1
                    in
                    ( { model | grid = newMatrix, flags = model.flags + flagMod }
                    , Cmd.none
                    )

        -- Revealing the neighbours (if possible)
        Dig cell ->
            ( updateDig model cell, Cmd.none )

        GenerateBoard rev locs ->
            ( { model
                | grid =
                    generateGrid model.size locs
                        |> reveal [ rev ]
                , gameState = Play
                , bombs = List.length locs
              }
            , Cmd.none
            )

        Tick _ ->
            ( { model | time = Time.millisToPosix (Time.posixToMillis model.time + 100) }
            , Cmd.none
            )

        ChangeSize newSize ->
            ( { model | size = newSize }
            , Cmd.none
            )

        Start loc ->
            ( model
            , Random.generate
                (GenerateBoard loc)
                (generateBombs (model.size * model.size // 5) model.size loc)
            )

        StartAgain ->
            ( { grid = Matrix.empty
              , gameState = Between
              , time = Time.millisToPosix 0
              , size = model.size
              , flags = 0
              , bombs = 0
              }
            , Cmd.none
            )

        DoNothing ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ h1 [ id "title" ] [ text "Minesweep", span [ id "elm" ] [ text "elm" ] ]
        , case model.gameState of
            Between ->
                viewFakeGrid model.size

            _ ->
                viewGrid model.grid
        , case model.gameState of
            Between ->
                section [ class "content", id "introduction" ]
                    [ h2 [] [ text "Hello!" ]
                    , p []
                        [ text "Welcome to a cool, little minesweeper game created in Elm by George Ungureanu Vranceanu. Usual rules apply:"
                        , ul []
                            [ li [] [ text "Left click to reveal the square (first try is guaranteed to be safe)" ]
                            , li [] [ text "Right click to mark it as a bomb (put a little flag over it" ]
                            , li [] [ text "Left and right click at the same time to uncover all the neighbours (provided you put down the required number of flags)" ]
                            ]
                        ]
                    , p [] [ text "Use the slider below to set the desired size and then click on the grid to start playing the game:" ]
                    , div [ id "slider-container" ]
                        [ viewSlider model.size
                        , h3 [ class "stats" ] [ text (String.fromInt model.size ++ " x " ++ String.fromInt model.size) ]
                        , h3 [ class "stats" ]
                            [ text (String.fromInt (model.size * model.size // 5) ++ " ")
                            , span [] [ img [ id "paragraph-mine", src "img/mine.png" ] [] ]
                            ]
                        ]
                    , h2 [] [ text "Good Luck!" ]
                    ]

            End False ->
                section
                    [ class "content end" ]
                    [ h3 [] [ text "Sorry, you blew up!" ]
                    , p [] [ text "You misplaced flags in the highlighted squares." ]
                    , button [ class "again-button", onClick StartAgain ] [ text "Start Again" ]
                    ]

            End True ->
                let
                    sizeString : String
                    sizeString =
                        String.fromInt model.size
                in
                section [ class "content end" ]
                    [ h3 []
                        [ text
                            ("Congratulations! You solved the "
                                ++ sizeString
                                ++ "x"
                                ++ sizeString
                                ++ " grid in "
                                ++ timeToString model.time
                            )
                        ]
                    , button [ class "again-button", onClick StartAgain ] [ text "Start Again" ]
                    ]

            Play ->
                section [ class "content", id "play" ]
                    [ img [ id "clock", src "img/clock.png" ] []
                    , h3 [ id "time" ] [ text <| timeToString <| model.time ]
                    , h3 []
                        [ span
                            (if model.flags > model.bombs then
                                [ id "too-many" ]

                             else
                                []
                            )
                            [ text (String.fromInt model.flags ++ " ") ]
                        , span [] [ img [ src "img/flag.png", class "play-img" ] [] ]
                        , text " / "
                        , text (String.fromInt model.bombs ++ " ")
                        , span [] [ img [ src "img/mine.png", class "play-img" ] [] ]
                        ]
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
                td [] [ img [ src "img/explosion.png" ] [] ]

            Bomb False ->
                td [] [ img [ src "img/mine.png" ] [] ]

            Around 0 flagged ->
                td
                    (class "around-0"
                        :: (if flagged then
                                [ class "wrong-flag" ]

                            else
                                []
                           )
                    )
                    []

            Around bombs flagged ->
                td
                    ([ class ("around-" ++ String.fromInt bombs), onBothClicks (Dig cell) ]
                        ++ (if flagged then
                                [ class "wrong-flag" ]

                            else
                                []
                           )
                    )
                    [ text (String.fromInt bombs) ]

    else
        td [ class "hidden", onClick (Clicked cell), onRightClick (Flag cell) ]
            [ if cell.flagged then
                img [ src "img/flag.png" ] []

              else
                text ""
            ]


timeToString : Time.Posix -> String
timeToString time =
    let
        minute =
            let
                minuteUnformatted : String
                minuteUnformatted =
                    String.fromInt (Time.toMinute Time.utc time)
            in
            if String.length minuteUnformatted < 2 then
                "0" ++ minuteUnformatted

            else
                minuteUnformatted

        second =
            let
                secondUnformatted : String
                secondUnformatted =
                    String.fromInt (Time.toSecond Time.utc time)
            in
            if String.length secondUnformatted < 2 then
                "0" ++ secondUnformatted

            else
                secondUnformatted

        milis =
            String.left 1 <| String.fromInt (Time.toMillis Time.utc time)
    in
    minute ++ ":" ++ second ++ "." ++ milis


viewSlider : Int -> Html Msg
viewSlider val =
    input
        [ id "size-slider"
        , type_ "range"
        , Html.Attributes.min "5"
        , Html.Attributes.max "20"
        , value (String.fromInt val)
        , Html.Events.onInput (ChangeSize << Maybe.withDefault 14 << String.toInt)
        ]
        []


viewGrid : Matrix.Matrix Cell -> Html Msg
viewGrid matrix =
    table [ id "grid", preventContextMenu DoNothing ]
        (matrix
            |> Matrix.map convertCell
            |> Matrix.toLists
            |> List.map (tr [])
        )


convertFakeCell : Loc -> Html Msg
convertFakeCell loc =
    td [ class "hidden", onClick (Start loc) ] []


viewFakeGrid : Int -> Html Msg
viewFakeGrid size =
    let
        matrix : Matrix.Matrix Loc
        matrix =
            Matrix.initialize size size identity
    in
    table [ id "grid", preventContextMenu DoNothing ]
        (matrix
            |> Matrix.map convertFakeCell
            |> Matrix.toLists
            |> List.map (tr [])
        )
