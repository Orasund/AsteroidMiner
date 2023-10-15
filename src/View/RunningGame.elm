module View.RunningGame exposing (Model, Msg(..), Status(..), gameArea, guiArea, init, subscriptions, update)

import Building exposing (BuildingType(..), GroundType(..), Volume(..))
import Config exposing (floorCosts, fps, size, spriteSize)
import Data.Comet as Comet exposing (Comet)
import Data.Game as Game exposing (Game)
import Data.Map exposing (Map, Square, SquareType(..))
import Data.ToolSelection as ToolSelection exposing (ToolSelection(..))
import Dict
import Grid.Bordered as Grid exposing (Error(..))
import Html exposing (Html)
import Lib.Neighborhood as Neighborhood
import Location exposing (Angle(..))
import PixelEngine
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
    case Dict.get pos model.game.map of
        Just ( BuildingSquare building, maybeItem ) ->
            if building.sort |> Building.canBreak then
                { model
                    | game =
                        { game
                            | map =
                                game.map
                                    |> Dict.insert pos (Game.emptySquare maybeItem)
                        }
                }

            else
                model

        _ ->
            model


placeSquare : BuildingType -> ( Int, Int ) -> Model -> Model
placeSquare building position ({ game } as model) =
    let
        defaultCase : Model
        defaultCase =
            model

        updateSquare : BuildingType -> Square -> Square
        updateSquare b square =
            case square of
                ( GroundSquare (Mountain { big }), _ ) ->
                    b
                        |> Game.newBuilding True
                            (if big then
                                Config.mineVolume * 2

                             else
                                Config.mineVolume
                            )

                ( _, maybeItem ) ->
                    Game.newBuilding maybeItem 0 b
    in
    case game.map |> Neighborhood.fromPosition position of
        ( Just _, _ ) ->
            game.map
                |> Dict.update position (Maybe.map (updateSquare building))
                |> (\m ->
                        { model
                            | game = { game | map = m }
                        }
                   )

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
                                |> Dict.insert position ( GroundSquare Dirt, False )
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


gameArea : List ( ( Int, Int ), Tile Msg ) -> Model -> Html Msg
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
        |> List.singleton
        |> PixelEngine.toHtml
            { options = Just Config.defaultOptions
            , width = (toFloat <| Config.size) * Config.spriteSize
            }


guiArea : Model -> Html Msg
guiArea model =
    GUI.view model.gui
        |> Html.map GuiSpecific
