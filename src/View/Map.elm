module View.Map exposing (view, viewSquareType)

import Building exposing (BuildingType(..), GroundType(..))
import Data
import Data.Game as Game
import Data.Map exposing (Map, Square, SquareType(..))
import Data.ToolSelection exposing (ToolSelection(..))
import Dict
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


view :
    { onClick : ( Int, Int ) -> msg
    , selected : Maybe ToolSelection
    , inventory : Int
    }
    -> Map
    -> List ( ( Int, Int ), Tile msg )
view { onClick, selected } map =
    List.range 0 (Data.size - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (Data.size - 1)
                    |> List.map (Tuple.pair x)
            )
        |> List.filterMap
            (\pos ->
                map
                    |> Dict.get pos
                    |> (\maybeSquare ->
                            case
                                ( maybeSquare
                                , selected
                                    |> Maybe.map (\s -> Game.isValid s pos map)
                                    |> Maybe.withDefault False
                                )
                            of
                                ( Just square, valid ) ->
                                    square
                                        |> viewSquare
                                            { position = pos
                                            , onClick = onClick
                                            , valid = Just valid
                                            }
                                        |> Tuple.pair pos
                                        |> Just

                                ( Nothing, True ) ->
                                    (Tileset.valid |> Tile.clickable (onClick pos))
                                        |> Tuple.pair pos
                                        |> Just

                                _ ->
                                    Nothing
                       )
            )
