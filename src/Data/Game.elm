module Data.Game exposing (Game, emptySquare, getBuildingType, getGroundType, isBuildingType, isGroundType, isValid, newBuilding, solveConflict, updateBuilding)

import Building exposing (BuildingType(..), Volume(..))
import Building.ColoredConveyorBelt as ColoredConveyorBelt
import Building.Container as Container
import Building.ConveyorBelt as ConveyorBelt
import Building.Merger as Merger
import Building.Mine as Mine
import Building.Sorter as Sorter
import Data exposing (mineVolume)
import Data.Comet exposing (Comet)
import Data.Item exposing (Item)
import Data.Map as Map exposing (GroundType(..), Map, Neighborhood, Square)
import Lib.Map as Map exposing (SquareType(..))
import Lib.Neighborhood as Neighborhood
import View exposing (ToolSelection)


type alias Game =
    { comet : Comet
    , map : Map
    , bag : Maybe Item
    , debts : Int
    }


solveConflict : BuildingType -> Neighborhood -> Item -> { item : Item, value : Int } -> Bool
solveConflict sort neigh =
    case sort of
        ColoredConveyorBelt _ _ ->
            ColoredConveyorBelt.canStore neigh

        ConveyorBelt _ ->
            ConveyorBelt.canStore neigh

        Mine ->
            Mine.canStore neigh

        Container _ ->
            Container.canStore neigh

        Merger ->
            Merger.canStore neigh

        Sorter ->
            Sorter.canStore neigh


updateBuilding : BuildingType -> ({ value : Int, item : Maybe Item } -> Neighborhood -> Map.Command)
updateBuilding sort =
    case sort of
        ColoredConveyorBelt color dir ->
            always <| ColoredConveyorBelt.update color dir

        ConveyorBelt code ->
            always <| ConveyorBelt.update code

        Mine ->
            Mine.update

        Container bool ->
            Container.update bool

        Merger ->
            always <| Merger.update

        Sorter ->
            always <| Sorter.update


newBuilding : Maybe Item -> Int -> BuildingType -> Square
newBuilding maybeItem value buildingType =
    ( BuildingSquare { value = value, sort = buildingType }, maybeItem )


emptySquare : Maybe Item -> Square
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


isValidMinePos : Neighborhood.Neighborhood (Maybe Square) -> Bool
isValidMinePos neigh =
    [ neigh.up, neigh.left, neigh.right, neigh.down ]
        |> List.any
            (Maybe.map
                (\( a, _ ) ->
                    case a of
                        GroundSquare (Mountain _) ->
                            True

                        _ ->
                            False
                )
                >> Maybe.withDefault False
            )


isValid : ToolSelection -> ( Int, Int ) -> Map -> Bool
isValid selected position map =
    case map |> Neighborhood.fromPosition position of
        Ok ( Just square, neigh ) ->
            case ( selected, square ) of
                ( View.Floor, _ ) ->
                    False

                ( View.Delete, ( GroundSquare _, _ ) ) ->
                    False

                ( View.Bag Nothing, ( GroundSquare _, Just _ ) ) ->
                    True

                ( View.Bag Nothing, ( GroundSquare _, Nothing ) ) ->
                    False

                ( View.Bag (Just _), ( GroundSquare _, _ ) ) ->
                    False

                ( View.Mine, ( GroundSquare Dirt, _ ) ) ->
                    False

                ( _, ( GroundSquare Dirt, _ ) ) ->
                    True

                ( View.Mine, ( GroundSquare (Mountain _), _ ) ) ->
                    neigh |> isValidMinePos

                ( _, ( GroundSquare (Mountain _), _ ) ) ->
                    False

                ( View.Delete, ( BuildingSquare { sort }, _ ) ) ->
                    sort |> Building.canBreak

                ( View.Bag (Just a), ( BuildingSquare { sort, value }, Just b ) ) ->
                    if solveConflict sort (neigh |> Neighborhood.map (Maybe.andThen getBuildingType)) a { item = b, value = value } then
                        sort |> Building.isInput

                    else
                        False

                ( _, _ ) ->
                    False

        Ok ( Nothing, neigh ) ->
            (selected == View.Floor)
                && (neigh
                        |> Neighborhood.toList
                        |> List.any (Tuple.second >> (/=) Nothing)
                   )

        _ ->
            False
