module Building.Sorter exposing (canStore, update)

import Building exposing (BuildingType(..))
import Data.Item exposing (Item)
import Data.Map exposing (Command, Neighborhood)
import Lib.Command as Command
import Lib.Neighborhood as Neighborhood


canStore : Neighborhood -> Item -> { value : Int, item : Item } -> Bool
canStore _ _ _ =
    False


update : Neighborhood -> Command
update neigh =
    neigh
        |> Neighborhood.toList
        |> List.filterMap
            (\( dir, ( a, _ ) ) ->
                case a of
                    Just Pipe ->
                        Just <| Command.send dir

                    Just (Container _) ->
                        Just <| Command.send dir

                    _ ->
                        Nothing
            )
        |> Command.batch
