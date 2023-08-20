module View.Map exposing (view, viewItem, viewSquareType)

import Building exposing (BuildingType(..), Code(..))
import Data.Game as Game
import Data.Item exposing (Item(..))
import Data.Map as Map exposing (GroundType(..), Map, Square)
import Data.ToolSelection exposing (BeltColor(..), ToolSelection(..))
import Grid.Bordered as Grid
import Lib.Map exposing (SquareType(..))
import PixelEngine.Tile as Tile exposing (Tile)
import View.Tileset as Tileset


viewSquareType : Map.SquareType -> Tile msg
viewSquareType squareType =
    case squareType of
        GroundSquare groundType ->
            case groundType of
                Dirt ->
                    Tileset.ground

                Mountain { big } ->
                    if big then
                        Tileset.bigMountain

                    else
                        Tileset.mountain

                OreGround ->
                    Tileset.oreGround

        BuildingSquare buildingType ->
            case buildingType.sort of
                Building.Mine ->
                    Tileset.mine

                Building.ConveyorBelt code ->
                    Tileset.conveyorBelt code

                Building.ColoredConveyorBelt color direction ->
                    Tileset.coloredConveyorBelt color direction

                Building.Pipe ->
                    Tileset.conveyorBelt (Try Blue)

                Building.Container volume ->
                    Tileset.container volume

                Building.Merger ->
                    Tileset.merger

                Building.Sorter ->
                    Tileset.sorter


viewItem : Item -> Tile msg
viewItem item =
    case item of
        Stone ->
            Tileset.stone


viewSquare :
    { position : ( Int, Int )
    , onClick : ( Int, Int ) -> msg
    , valid : Maybe Bool
    }
    -> Square
    -> Tile msg
viewSquare { position, onClick, valid } ( squareType, maybeItem ) =
    let
        item : Maybe (Tile msg)
        item =
            maybeItem |> Maybe.map viewItem
    in
    (case item of
        Just tile ->
            Tile.multipleTiles
                [ viewSquareType squareType, tile ]

        Nothing ->
            viewSquareType squareType
    )
        |> (case valid of
                Just bool ->
                    \t ->
                        if bool then
                            Tile.multipleTiles [ t, Tileset.valid ]
                                |> Tile.clickable (onClick position)

                        else
                            t

                Nothing ->
                    Tile.clickable (onClick position)
           )


view : { onClick : ( Int, Int ) -> msg, selected : ToolSelection, inventory : Int } -> Map -> List ( ( Int, Int ), Tile msg )
view { onClick, selected } map =
    map
        |> Grid.map
            (\pos maybeSquare ->
                case ( maybeSquare, Game.isValid selected pos map ) of
                    ( Just square, valid ) ->
                        square
                            |> viewSquare
                                { position = pos
                                , onClick = onClick
                                , valid = Just valid
                                }
                            |> Just

                    ( Nothing, True ) ->
                        Just (Tileset.valid |> Tile.clickable (onClick pos))

                    _ ->
                        Nothing
            )
        >> Grid.toList
