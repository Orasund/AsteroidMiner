module Building.Sorter exposing (canStore, update)

import Building exposing (BuildingType(..))
import Data.Map exposing (Neighborhood)
import Lib.Map exposing (SingleCommand(..))
import Lib.Neighborhood as Neighborhood


canStore : Neighborhood -> { value : Int } -> Bool
canStore _ _ =
    False


update : Neighborhood -> List SingleCommand
update neigh =
    neigh
        |> Neighborhood.toList
        |> List.filterMap
            (\( dir, ( a, _ ) ) ->
                case a of
                    Just Pipe ->
                        Send dir |> Just

                    Just (Container _) ->
                        Send dir |> Just

                    _ ->
                        Nothing
            )
