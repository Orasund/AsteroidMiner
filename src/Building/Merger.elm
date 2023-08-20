module Building.Merger exposing (canStore, update)

import Building exposing (BuildingType(..))
import Data.Map exposing (Neighborhood, SingleCommand(..))
import Lib.Neighborhood as Neighborhood


canStore : Neighborhood -> { value : Int } -> Bool
canStore _ _ =
    False


update : Neighborhood -> List SingleCommand
update neigh =
    neigh
        |> List.filterMap
            (\( dir, ( a, _ ) ) ->
                case a of
                    Just Pipe ->
                        Just <| Send dir

                    Just Sorter ->
                        Just <| Send dir

                    _ ->
                        Nothing
            )
