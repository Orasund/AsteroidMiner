module Building.Pipe exposing (..)

import Building exposing (BuildingType(..))
import Data.Map exposing (Neighborhood)
import Lib.Command exposing (SingleCommand(..))
import Lib.Neighborhood as Neighborhood


canStore : Neighborhood -> { value : Int } -> Bool
canStore _ _ =
    False


update : Neighborhood -> List SingleCommand
update neigh =
    let
        inputs =
            filterMap (\building _ -> building |> Building.isInput)

        friends =
            filterMap (\building maybeItem -> building == Pipe && maybeItem == False)

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
        |> List.map Send
