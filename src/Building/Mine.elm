module Building.Mine exposing (canStore, update)

import Building exposing (BuildingType(..))
import Data.Map exposing (Neighborhood)
import Lib.Command exposing (SingleCommand(..))
import Lib.Neighborhood as Neighborhood


canStore : Neighborhood -> { value : Int } -> Bool
canStore _ _ =
    False


update : { value : Int, item : Bool } -> Neighborhood -> List SingleCommand
update { value } neigh =
    if value == 0 then
        [ Destroy ]

    else
        neigh
            |> Neighborhood.toList
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
