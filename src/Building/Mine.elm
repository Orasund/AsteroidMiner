module Building.Mine exposing (canStore, update)

import Building exposing (BuildingType(..))
import Data.Item exposing (Item)
import Data.Map exposing (Command, Neighborhood)
import Lib.Command as Command
import Lib.Neighborhood as Neighborhood


canStore : Neighborhood -> Item -> { value : Int, item : Item } -> Bool
canStore _ _ _ =
    False


update : { value : Int, item : Maybe Item } -> Neighborhood -> Command
update { value } neigh =
    if value == 0 then
        Command.destroy

    else
        neigh
            |> Neighborhood.toList
            |> List.filterMap
                (\( dir, ( a, _ ) ) ->
                    case a of
                        Just (ColoredConveyorBelt _ d) ->
                            if dir == d then
                                Just <| Command.send dir

                            else
                                Nothing

                        Just Pipe ->
                            Just <| Command.send dir

                        Just Sorter ->
                            Just <| Command.send dir

                        _ ->
                            Nothing
                )
            |> Command.batch
