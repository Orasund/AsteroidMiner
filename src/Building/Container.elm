module Building.Container exposing (canStore, update)

import Building exposing (BuildingType(..), Volume(..))
import Data exposing (maxValue)
import Data.Map exposing (Neighborhood, SingleCommand(..))
import Lib.Neighborhood as Neighborhood


canStore : Neighborhood -> { value : Int } -> Bool
canStore _ { value } =
    value < maxValue


update : Volume -> { value : Int, item : Bool } -> Neighborhood -> List SingleCommand
update volume { value } neigh =
    let
        transition : Volume -> List SingleCommand
        transition v =
            if volume == v then
                []

            else
                [ Transition <| Container v ]
    in
    neigh
        |> Neighborhood.toList
        |> List.filterMap
            (\( dir, ( maybeBuilding, _ ) ) ->
                maybeBuilding
                    |> Maybe.andThen
                        (\building ->
                            if Building.isOutput building then
                                ( dir, maybeBuilding ) |> Just

                            else
                                Nothing
                        )
            )
        |> List.map (Tuple.first >> Send)
        |> (++)
            (if value == 0 then
                transition Empty

             else if value < maxValue // 2 then
                transition HalfEmpty

             else if value == maxValue then
                transition Full

             else
                transition HalfFull
            )
