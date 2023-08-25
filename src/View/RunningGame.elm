module View.RunningGame exposing (Model, Msg(..), Status(..), areas, gameArea, guiArea, init, subscriptions, update)

import Building exposing (BuildingType(..), GroundType(..), Volume(..))
import Color
import Data exposing (floorCosts, fps, size, spriteSize)
import Data.Comet as Comet exposing (Comet)
import Data.Game as Game exposing (Game)
import Data.Map exposing (Map, Square, SquareType(..))
import Data.ToolSelection as ToolSelection exposing (ToolSelection(..))
import Grid.Bordered as Grid exposing (Error(..))
import Lib.Neighborhood as Neighborhood
import Location exposing (Angle(..))
import PixelEngine exposing (Area)
import PixelEngine.Tile exposing (Tile)
import Random exposing (Seed)
import Time
import View.GUI as GUI
import View.Map as Map
import View.Tileset as Tileset exposing (tileset)



----------------------
-- Model
----------------------


type Status
    = Running
    | Won
    | Lost


type alias Model =
    { game : Game
    , seed : Seed
    , gui : GUI.Model
    , inventory : Int
    , status : Status
    , winCondition : Int
    }


type Msg
    = TimePassed
    | SquareClicked ( Int, Int )
    | GuiSpecific GUI.Msg



----------------------
-- Init
----------------------


init : { winCondition : Int, map : Map, seed : Seed } -> Model
init { winCondition, map, seed } =
    let
        ( angle, newSeed ) =
            seed
                |> Random.step (Random.float 0 (2 * pi) |> Random.map Angle)

        comet : Comet
        comet =
            Comet.new
                { angle = angle
                , moveClockwise = True
                }

        game : Game
        game =
            { comet = comet
            , map = map
            , debts = 0
            }
    in
    { game = game
    , seed = newSeed
    , gui = GUI.init
    , inventory = 0
    , status = Running
    , winCondition = winCondition
    }



----------------------
-- Update
----------------------


timePassed : Model -> Model
timePassed ({ game, seed, winCondition } as model) =
    let
        ( ( comet, map ), newSeed ) =
            Random.step (game.comet |> Comet.update game.map) seed

        ( newMap, inventory ) =
            map
                |> Data.Map.update
                    { empty = Dirt
                    , update =
                        \pos ->
                            case Neighborhood.fromPosition pos game.map of
                                ( Just ( BuildingSquare { value, sort }, maybeItem ), neigh ) ->
                                    Game.updateBuilding
                                        sort
                                        { value = value, item = maybeItem }
                                        (neigh
                                            |> List.map
                                                (Tuple.mapSecond
                                                    (\maybe ->
                                                        ( maybe |> Maybe.andThen Game.getBuildingType
                                                        , maybe |> Maybe.map Tuple.second |> Maybe.withDefault False
                                                        )
                                                    )
                                                )
                                        )

                                _ ->
                                    []
                    , canStore =
                        \pos ->
                            case Neighborhood.fromPosition pos game.map of
                                ( Just ( BuildingSquare { sort }, _ ), neigh ) ->
                                    always <|
                                        Game.solveConflict
                                            sort
                                            (neigh
                                                |> List.map
                                                    (Tuple.mapSecond
                                                        (\maybe ->
                                                            ( maybe |> Maybe.andThen Game.getBuildingType
                                                            , maybe |> Maybe.map Tuple.second |> Maybe.withDefault False
                                                            )
                                                        )
                                                    )
                                            )

                                _ ->
                                    \_ _ -> False
                    }
                |> Tuple.mapSecond ((+) -game.debts)

        status : Status
        status =
            if inventory >= winCondition then
                Won

            else
                Running

        newGame : Game
        newGame =
            { game
                | comet = comet
                , map = newMap
            }
    in
    { model
        | game = newGame
        , seed = newSeed
        , inventory = inventory
        , status = status
    }


deleteSqaure : ( Int, Int ) -> Model -> Model
deleteSqaure pos ({ game } as model) =
    let
        updateFun : Maybe Square -> Result () (Maybe Square)
        updateFun maybeElem =
            case maybeElem of
                Just ( BuildingSquare building, maybeItem ) ->
                    if building.sort |> Building.canBreak then
                        Ok <| Just <| Game.emptySquare maybeItem

                    else
                        Err ()

                _ ->
                    Err ()
    in
    { model
        | game =
            { game
                | map =
                    game.map
                        |> (Grid.ignoringErrors <| Grid.update pos updateFun)
            }
    }


placeSquare : BuildingType -> ( Int, Int ) -> Model -> Model
placeSquare building position ({ game } as model) =
    let
        defaultCase : Model
        defaultCase =
            model

        updateSquare : BuildingType -> Maybe Square -> Result () (Maybe Square)
        updateSquare b maybeSquare =
            case maybeSquare of
                Just ( GroundSquare (Mountain { big }), _ ) ->
                    b
                        |> Game.newBuilding True
                            (if big then
                                Data.mineVolume * 2

                             else
                                Data.mineVolume
                            )
                        |> Just
                        |> Ok

                Just ( _, maybeItem ) ->
                    Ok <|
                        Just <|
                            Game.newBuilding maybeItem 0 b

                Nothing ->
                    Err ()
    in
    case game.map |> Neighborhood.fromPosition position of
        ( Just _, _ ) ->
            case game.map |> Grid.update position (updateSquare building) of
                Ok m ->
                    { model
                        | game = { game | map = m }
                    }

                Err _ ->
                    defaultCase

        _ ->
            defaultCase


squareClicked : ( Int, Int ) -> Model -> Model
squareClicked position ({ gui, game } as model) =
    let
        build : BuildingType -> Model
        build tool =
            placeSquare tool position model

        placeFloor : Model
        placeFloor =
            { model
                | game =
                    { game
                        | map =
                            game.map
                                |> Grid.ignoringErrors
                                    (Grid.insert position ( GroundSquare Dirt, False ))
                        , debts =
                            game.debts + floorCosts
                    }
            }
    in
    case gui.selected of
        Just ToolSelection.Delete ->
            deleteSqaure position model

        Just ToolSelection.Mine ->
            Building.Mine |> build

        Just ToolSelection.Pipe ->
            Building.Pipe |> build

        Just ToolSelection.Container ->
            Building.Container Empty |> build

        Just ToolSelection.Merger ->
            Building.Merger |> build

        Just ToolSelection.Sorter ->
            Building.Sorter |> build

        Just ToolSelection.Floor ->
            placeFloor

        Nothing ->
            model


update : Msg -> Model -> Model
update msg =
    case msg of
        TimePassed ->
            timePassed

        SquareClicked position ->
            squareClicked position

        GuiSpecific guiMsg ->
            \({ gui } as model) ->
                { model | gui = GUI.update guiMsg gui }



----------------------
-- Subscriptions
----------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        second : Float
        second =
            1000
    in
    Time.every (second / fps) (always TimePassed)



----------------------
-- Areas
----------------------


viewComet : Comet -> ( ( Int, Int ), Tile msg )
viewComet comet =
    ( Comet.position comet, Tileset.comet )


gameArea : List ( ( Int, Int ), Tile Msg ) -> Model -> Area Msg
gameArea content model =
    List.concat
        [ Map.view
            { onClick = SquareClicked
            , selected = model.gui.selected
            , inventory = model.inventory
            }
            model.game.map
        , [ viewComet model.game.comet ]
        , content
        ]
        |> PixelEngine.tiledArea
            { rows = size
            , tileset = tileset
            , background =
                PixelEngine.imageBackground
                    { source = "background.png"
                    , width = spriteSize
                    , height = spriteSize
                    }
            }


guiArea : Model -> Area Msg
guiArea model =
    PixelEngine.imageArea
        { height = 3 * spriteSize
        , background =
            PixelEngine.colorBackground <|
                Color.rgb255 20 12 28
        }
        (GUI.view model.inventory model.gui)
        |> PixelEngine.mapArea GuiSpecific


areas : List ( ( Int, Int ), Tile Msg ) -> Model -> List (Area Msg)
areas content model =
    [ gameArea content model
    , guiArea model
    ]
