module View.RunningGame exposing (Model, Msg(..), Status(..), areas, init, subscriptions, update)

import Building exposing (BuildingType(..), Code(..), Volume(..))
import Color
import Data exposing (floorCosts, fps, size, spriteSize)
import Data.Comet as Comet exposing (Comet)
import Data.Game as Game exposing (Game)
import Data.Item exposing (Item(..))
import Data.Map as Map exposing (GroundType(..), Map, Square)
import Grid.Bordered as Grid exposing (Error(..))
import Lib.Command as Command
import Lib.Map exposing (SquareType(..))
import Lib.Neighborhood as Neighborhood
import Location exposing (Angle(..))
import PixelEngine exposing (Area)
import PixelEngine.Tile exposing (Tile)
import Position
import Random exposing (Seed)
import Time
import View exposing (ToolSelection(..))
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
            Comet.new angle

        game : Game
        game =
            { comet = comet
            , map = map
            , bag = Nothing
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
                |> Map.update
                    { empty = Dirt
                    , update =
                        \pos ->
                            case Neighborhood.fromPosition pos game.map of
                                Ok ( Just ( BuildingSquare { value, sort }, maybeItem ), neigh ) ->
                                    Game.updateBuilding
                                        sort
                                        { value = value, item = maybeItem }
                                        (neigh
                                            |> Neighborhood.map
                                                (Maybe.andThen Game.getBuildingType)
                                        )

                                _ ->
                                    Command.idle
                    , canStore =
                        \pos ->
                            case Neighborhood.fromPosition pos game.map of
                                Ok ( Just ( BuildingSquare { sort }, _ ), neigh ) ->
                                    always <|
                                        Game.solveConflict
                                            sort
                                            (neigh
                                                |> Neighborhood.map
                                                    (Maybe.andThen Game.getBuildingType)
                                            )

                                _ ->
                                    always <| always <| always <| False
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
                Just ( GroundSquare Mountain, _ ) ->
                    Ok <|
                        Just <|
                            Game.newBuilding (Just Stone) b

                Just ( _, maybeItem ) ->
                    Ok <|
                        Just <|
                            Game.newBuilding maybeItem b

                Nothing ->
                    Err ()
    in
    case game.map |> Neighborhood.fromPosition position of
        Ok ( Just _, _ ) ->
            case game.map |> Grid.update position (updateSquare building) of
                Ok m ->
                    { model
                        | game = { game | map = m }
                    }

                Err _ ->
                    defaultCase

        _ ->
            defaultCase


pickUpSquare : ( Int, Int ) -> Model -> Model
pickUpSquare position ({ gui, game } as model) =
    let
        newModel : Model
        newModel =
            case
                game.map
                    |> Grid.get position
            of
                Ok (Just ( square, Just item )) ->
                    { model
                        | game =
                            { game
                                | bag = Just item
                                , map =
                                    game.map
                                        |> Grid.ignoringErrors
                                            (Grid.update position
                                                (always <|
                                                    Ok <|
                                                        Just <|
                                                            ( square, Nothing )
                                                )
                                            )
                            }
                        , gui = gui |> GUI.select (Bag <| Just <| item)
                    }

                _ ->
                    model
    in
    newModel


insertItem : Item -> ( Int, Int ) -> Model -> Model
insertItem item position ({ gui, game } as model) =
    let
        newModel : Model
        newModel =
            case
                game.map
                    |> Grid.get position
            of
                Ok (Just ( BuildingSquare b, Just i )) ->
                    if i == item then
                        { model
                            | game =
                                { game
                                    | bag = Nothing
                                    , map =
                                        game.map
                                            |> Grid.ignoringErrors
                                                (Grid.update position
                                                    (always <|
                                                        Ok <|
                                                            Just <|
                                                                ( BuildingSquare { b | value = b.value + 1 }, Just i )
                                                    )
                                                )
                                }
                            , gui = gui |> GUI.select (Bag <| Nothing)
                        }

                    else
                        model

                _ ->
                    model
    in
    newModel


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
                                    (Grid.insert position ( GroundSquare Dirt, Nothing ))
                        , debts =
                            game.debts + floorCosts
                    }
            }
    in
    case gui.selected of
        View.Delete ->
            deleteSqaure position model

        View.Bag Nothing ->
            pickUpSquare position model

        View.Bag (Just item) ->
            insertItem item position model

        View.Mine ->
            Building.Mine |> build

        View.ConveyorBelt ->
            Building.ConveyorBelt Invalid |> build

        View.Container ->
            Building.Container Empty |> build

        View.Merger ->
            Building.Merger |> build

        View.Sorter ->
            Building.Sorter |> build

        View.Floor ->
            placeFloor


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


areas : List ( ( Int, Int ), Tile Msg ) -> Model -> List (Area Msg)
areas content { game, gui, inventory } =
    let
        { map, comet, bag } =
            game
    in
    [ PixelEngine.tiledArea
        { rows = size - 3
        , tileset = tileset
        , background =
            PixelEngine.imageBackground
                { source = "background.png"
                , width = spriteSize
                , height = spriteSize
                }
        }
      <|
        List.concat
            [ Map.view
                { onClick = SquareClicked
                , selected = gui.selected
                , inventory = inventory
                }
                map
            , [ viewComet comet ]
            , content
            ]
    , PixelEngine.imageArea
        { height = 3 * spriteSize
        , background =
            PixelEngine.colorBackground <|
                Color.rgb255 20 12 28
        }
        (GUI.view bag inventory gui)
        |> PixelEngine.mapArea GuiSpecific
    ]