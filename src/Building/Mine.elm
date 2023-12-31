module Building.Mine exposing (canStore, update)

import Building exposing (BuildingType(..))
import Data.Map exposing (Neighborhood, SingleCommand(..))
import Lib.Neighborhood as Neighborhood


canStore : Neighborhood -> { value : Int } -> Bool
canStore _ _ =
    False


update : { value : Int, item : Bool } -> Neighborhood -> List SingleCommand
update { value, item } neigh =
    if value == 0 && not item then
        [ Destroy ]

    else
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
