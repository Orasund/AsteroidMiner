module Data.Game exposing (Game, emptySquare, getBuildingType, getGroundType, isBuildingType, isGroundType, isValid, newBuilding, solveConflict, updateBuilding)

import Building exposing (BuildingType(..), GroundType(..), Volume(..))
import Building.Container as Container
import Building.Merger as Merger
import Building.Mine as Mine
import Building.Pipe as Pipe
import Building.Sorter as Sorter
import Data.Comet exposing (Comet)
import Data.Map exposing (Map, Neighborhood, SingleCommand, Square, SquareType(..))
import Data.ToolSelection as ToolSelection exposing (ToolSelection(..))
import Dict
import Lib.Neighborhood as Neighborhood


type alias Game =
    { comet : Comet
    , map : Map
    , debts : Int
    }


solveConflict : BuildingType -> Neighborhood -> { value : Int } -> Bool
solveConflict sort neigh =
    case sort of
        Building.Pipe ->
            Pipe.canStore neigh

        Building.Mine ->
            Mine.canStore neigh

        Building.Container _ ->
            Container.canStore neigh

        Building.Merger ->
            Merger.canStore neigh

        Building.Sorter ->
            Sorter.canStore neigh


updateBuilding :
    BuildingType
    -> ({ value : Int, item : Bool } -> Neighborhood -> List SingleCommand)
updateBuilding sort =
    case sort of
        Building.Pipe ->
            \_ -> Pipe.update

        Building.Mine ->
            Mine.update

        Building.Container bool ->
            Container.update bool

        Building.Merger ->
            always <| Merger.update

        Building.Sorter ->
            always <| Sorter.update


newBuilding : Bool -> Int -> BuildingType -> Square
newBuilding maybeItem value buildingType =
    ( BuildingSquare { value = value, sort = buildingType }, maybeItem )


emptySquare : Bool -> Square
emptySquare maybeItem =
    ( GroundSquare Dirt, maybeItem )


getBuildingType : Square -> Maybe BuildingType
getBuildingType square =
    case square of
        ( BuildingSquare { sort }, _ ) ->
            Just sort

        _ ->
            Nothing


isBuildingType : BuildingType -> Square -> Bool
isBuildingType bType =
    getBuildingType
        >> Maybe.map ((==) bType)
        >> Maybe.withDefault False


getGroundType : Square -> Maybe GroundType
getGroundType square =
    case square of
        ( GroundSquare g, _ ) ->
            Just g

        _ ->
            Nothing


isGroundType : GroundType -> Square -> Bool
isGroundType groundType =
    getGroundType
        >> Maybe.map ((==) groundType)
        >> Maybe.withDefault False


isValid : ToolSelection -> ( Int, Int ) -> Map -> Bool
isValid selected position map =
    case map |> Dict.get position of
        Just square ->
            case ( selected, square ) of
                ( ToolSelection.Floor, _ ) ->
                    False

                ( ToolSelection.Delete, ( GroundSquare _, _ ) ) ->
                    False

                ( ToolSelection.Mine, ( GroundSquare Dirt, _ ) ) ->
                    False

                ( _, ( GroundSquare Dirt, _ ) ) ->
                    True

                ( ToolSelection.Mine, ( GroundSquare (Mountain _), _ ) ) ->
                    map
                        |> Neighborhood.fromPosition position
                        |> Tuple.second
                        |> List.any
                            (\( _, maybe ) ->
                                case maybe of
                                    Just ( GroundSquare (Mountain _), _ ) ->
                                        False

                                    Nothing ->
                                        False

                                    _ ->
                                        True
                            )

                ( _, ( GroundSquare (Mountain _), _ ) ) ->
                    False

                ( ToolSelection.Delete, ( BuildingSquare { sort }, _ ) ) ->
                    sort |> Building.canBreak

                ( _, _ ) ->
                    False

        Nothing ->
            (selected == ToolSelection.Floor)
                && (map
                        |> Neighborhood.fromPosition position
                        |> Tuple.second
                        |> List.any (Tuple.second >> (/=) Nothing)
                   )
