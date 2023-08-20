module View.Map exposing (view, viewSquareType)

import Building exposing (BuildingType(..), GroundType(..))
import Data.Game as Game
import Data.ToolSelection exposing (ToolSelection(..))
import Grid.Bordered as Grid
import Lib.Map exposing (Map, Square, SquareType(..))
import PixelEngine.Tile as Tile exposing (Tile)
import View.Tileset as Tileset


viewSquareType : SquareType -> Tile msg
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

        BuildingSquare buildingType ->
            case buildingType.sort of
                Building.Mine ->
                    Tileset.mine

                Building.Pipe ->
                    Tileset.pipe

                Building.Container volume ->
                    Tileset.container volume

                Building.Merger ->
                    Tileset.merger

                Building.Sorter ->
                    Tileset.sorter


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
            if maybeItem then
                Just Tileset.stone

            else
                Nothing
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
