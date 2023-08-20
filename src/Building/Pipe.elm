module Building.Pipe exposing (..)

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
    let
        inputs =
            filterMap (\building _ -> building |> Building.isInput)

        friends =
            filterMap (\building maybeItem -> building == Pipe && maybeItem == Nothing)

        filterMap pred =
            neigh
                |> Neighborhood.toList
                |> List.filterMap
                    (\( dir, ( maybe, maybeItem ) ) ->
                        maybe
                            |> Maybe.andThen
                                (\building ->
                                    if pred building maybeItem then
                                        Just dir

                                    else
                                        Nothing
                                )
                    )
    in
    inputs
        ++ friends
        |> List.map Command.send
        |> Command.batch
