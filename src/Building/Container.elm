module Building.Container exposing (canStore, update)

import Building exposing (BuildingType(..), Volume(..))
import Data exposing (maxValue)
import Data.Item exposing (Item)
import Data.Map exposing (Command, Neighborhood)
import Lib.Command as Command
import Lib.Neighborhood as Neighborhood


canStore : Neighborhood -> Item -> { value : Int, item : Item } -> Bool
canStore _ input { value, item } =
    (input == item) && (value < maxValue)


update : Volume -> { value : Int, item : Maybe Item } -> Neighborhood -> Command
update volume { value } neigh =
    let
        transition : Volume -> Command
        transition v =
            if volume == v then
                Command.idle

            else
                Command.transition <| Container v
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
        |> List.map (Tuple.first >> Command.send)
        |> (::)
            (if value == 0 then
                transition Empty

             else if value < maxValue // 2 then
                transition HalfEmpty

             else if value == maxValue then
                transition Full

             else
                transition HalfFull
            )
        |> Command.batch
